;;;; kaleidoscope.lisp

(in-package #:kaleidoscope)

(defparameter *threads* 16)
(defparameter *width* 1024)
(defparameter *height* 1024)
(defparameter *style* (make-text-style :serif :roman 18))

(setf lparallel:*kernel* (lparallel:make-kernel *threads*))

(defun calculate-all-reflections (point eye mirrors polygon)
  (loop for times = 1 then (1+ times)
        for ray = (make-ray eye point) then (make-ray eye (reflection-point reflection))
        for reflection = (make-reflection ray mirrors times)
        while reflection
        collect (list :ref reflection :ray ray)
        while (not (region-intersects-region-p polygon (reflection-point reflection)))))

(defun calculate-reflection (x y eye mirrors polygon)
  (loop for times = 1 then (1+ times)
        for ray = (make-ray eye (make-point x y))
          then (make-ray eye (reflection-point reflection))
        for reflection = (make-reflection ray mirrors times)
        for in = (and reflection (region-contains-region-p polygon (reflection-point reflection)))
        while (and reflection (not in) (< times 10000))
        finally (return reflection)))

(defun calculate-reflections (frame reflections width height eye mirrors polygon)
  (let* ((div 8)
         (delta (/ div height)))
    (lparallel:pmapcar (lambda (coord)
                         (let ((i (first coord))
                               (j (second coord)))
                           (setf (aref reflections i j)
                                 (calculate-reflection i j eye mirrors polygon))
                           (when (and (= j height) (zerop (mod i div)))
                             (execute-frame-command frame (list 'com-increase-progress delta)))))
                       :parts *threads*
                       (loop for x from 0 to width
                             append (loop for y from 0 to height
                                          collect (list x y))))
    reflections))

(defun draw-reflection (frame canvas)
  (loop for ref in (calculate-all-reflections (kaleidoscope-point frame)
                                              (kaleidoscope-eye frame)
                                              (kaleidoscope-mirrors frame)
                                              (kaleidoscope-mirrors-polygon frame))
        do (draw (getf ref :ray) canvas)
           (draw (getf ref :ref) canvas)))

(defun draw-origin (width height)
  (clime:with-output-to-drawing-stream
      (stream :raster :pattern :recording-p nil
              :width width :height height)
    (draw-rectangle* stream 0 0 width height :ink +white+)
    (loop repeat 57
          for color = (make-rgb-color (random 1.0) (random 1.0) (random 1.0))
          for x = (random width)
          and y = (random height)
          and w = (/ (1+ (random width)) 6)
          if (zerop (random 2))
            do (draw-circle* stream x y w :ink color)
          else
            do (let ((w/2 (/ w 2)))
                 (draw-rectangle* stream
                                  (- x w/2) (- y w/2)
                                  (+ x w/2) (+ y w/2)
                                  :ink color)))))

(defun draw-all (frame canvas)
  (with-bounding-rectangle* (x0 y0 x1 y1) (kaleidoscope-mirrors-polygon frame)
    (let* ((dx (1+ (- x1 x0)))
           (dy (1+ (- y1 y0)))
           (origin (draw-origin dx dy))
           (output (make-image *width* *height*)))
      (loop for x from 0 below *width*
            do (loop for y from 0 below *height*
                     for ref = (aref (kaleidoscope-reflections frame) x y)
                     for ref-point = (and ref (reflection-point ref))
                     for yp = (and ref (round (- (point-y ref-point) y0)))
                     and xp = (and ref (round (- (point-x ref-point) x0)))
                     and times = (and ref (if (<= (reflection-times ref) 2)
                                              1
                                              (log (reflection-times ref) 3)))
                     if (and ref (< 0 xp dx) (< 0 yp dy))
                       do (multiple-value-bind (r g b a)
                              (clim-render::%rgba->vals (aref (clime:pattern-array origin)
                                                              yp xp))
                            (setf (aref (clime:pattern-array output) y x)
                                  (clim-render::%vals->rgba (round r times)
                                                            (round g times)
                                                            (round b times)
                                                            a)))
                     else
                       do (setf (aref (clime:pattern-array output) y x)
                                #x00000000)))
      (with-drawing-options (canvas :recording-p nil)
        (draw-pattern* canvas origin x0 y0)
        (draw-pattern* canvas output 0 0)
        (present (kaleidoscope-mirrors-polygon frame) 'mirrors :stream canvas)))))

(defclass graphical-view (view) ())
(defparameter +graphical-view+ (make-instance 'graphical-view))

(define-presentation-method present (eye (type eye) stream (view graphical-view)
                                         &key &allow-other-keys)
  (draw eye stream))

(define-presentation-method present (mirrors (type mirrors) stream (view graphical-view)
                                             &key &allow-other-keys)
  (draw mirrors stream))

(defun display-canvas (frame pane)
  (window-clear pane)
  (draw-all frame pane)
  (present (kaleidoscope-eye frame) 'eye :stream pane))

(defun display-progress (frame pane)
  (window-clear pane)
  (with-bounding-rectangle* (x0 y0 x1 y1) pane
    (declare (ignore x0 y0))
    (draw-rectangle* pane 0 0 (* x1 (reflections-progress-value frame)) (/ y1 2)
                     :ink +blue+)
    (let ((txt (format nil "Calculating reflections: ~3d/100"
                       (truncate (* 100 (reflections-progress-value frame))))))
      (draw-text* pane txt (/ (- x1 (text-size pane txt :text-style *style*)) 2) (- y1 20)
                  :text-style *style*))))

(define-application-frame reflections-progress ()
  ((value :initform 0 :accessor reflections-progress-value))
  (:panes (progress (make-pane 'clim-stream-pane
                               :name 'progress
                               :background +white+
                               :display-function #'display-progress
                               :display-time :command-loop)))
  (:layouts (default
             (vertically (:min-height 128 :max-height 128 :height 128
                          :min-width 1024 :max-width 1024 :width 1024)
               progress)))
  (:menu-bar nil))

(define-reflections-progress-command (com-increase-progress :name "Increase progress")
    ((increment 'number))
  (incf (reflections-progress-value *application-frame*) increment)
  (when (= 1 (incf (reflections-progress-value *application-frame*) increment))
    (frame-exit *application-frame*)))

(define-application-frame kaleidoscope ()
  ((mirrors :initarg :mirrors :accessor kaleidoscope-mirrors)
   (mirrors-polygon :initarg :mirrors-polygon :accessor kaleidoscope-mirrors-polygon)
   (kaleidoscope-reflections :initarg :reflections :accessor kaleidoscope-reflections)
   (eye :initarg :eye :accessor kaleidoscope-eye)
   (point :initarg :point :accessor kaleidoscope-point))
  (:panes (canvas (make-pane 'application-pane
                             :name 'canvas
                             :background +white+
                             :display-function 'display-canvas
                             :default-view +graphical-view+
                             :display-time t))
          (interactor :interactor))
  (:layouts (default
             (vertically (:min-height (* 5/4 *height*)
                          :max-height (* 5/4 *height*)
                          :height (* 5/4 *height*)
                          :min-width *width* :max-width *width* :width *width*)
               (4/5 canvas)
               (1/5 interactor))))
  (:menu-bar t))

(define-kaleidoscope-command (com-redraw :name "Redraw" :menu t) ()
  (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'canvas)
                        :force-p t))

(defun draw-mirror (pane x0 y0)
  (let (mirrors record records)
    (flet ((make-output-record (x y)
             (setf record (with-output-to-output-record (pane)
                            (with-output-as-presentation (pane nil 'figure)
                              (draw-line* pane x0 y0 x y :line-thickness 5
                                                         :ink +flipping-ink+))))))
      (block processor
        (tracking-pointer (pane)
          (:pointer-motion (&key window x y)
            (declare (ignore window))
            (when record
              (repaint-sheet pane (with-bounding-rectangle* (x1 y1 x2 y2) record
                                    (make-rectangle* (1- x1) (1- y1) (1+ x2) (1+ y2)))))
                           (make-output-record x y)
                           (loop for r in records do (replay r pane)
                                 finally (replay record pane)))
          (:pointer-button-release (&key event x y)
            (if (and (= x0 x) (= y0 y))
                (return-from processor (values x y))
                (when (= (pointer-event-button event) +pointer-left-button+)
                  (push (make-mirror (make-point x0 y0) (make-point x y)) mirrors)
                  (setf x0 x y0 y)
                  (push record records)
                  (when (= (length records) 2)
                    (return-from processor (values x y))))))
          (:pointer-button-press (&key event x y)
            (when (= (pointer-event-button event) +pointer-left-button+)
              (format *debug-io* "~S ~S ~S~%" (pointer-event-button event) x y)))))
      (when (= 2 (length mirrors))
        (push (make-mirror (line-end-point (first mirrors))
                           (line-start-point (second mirrors)))
              mirrors)
        (let* ((frame *application-frame*)
               (mirrors (reverse mirrors))
               (mirrors-polygon (make-mirrors mirrors)))
          (when (region-contains-position-p mirrors-polygon
                                            (point-x (kaleidoscope-eye frame))
                                            (point-y (kaleidoscope-eye frame)))
            (setf (kaleidoscope-mirrors frame) mirrors
                  (kaleidoscope-mirrors-polygon frame) mirrors-polygon
                  (kaleidoscope-reflections frame)
                  (calculate-reflections (find-application-frame 'reflections-progress)
                                         (kaleidoscope-reflections frame)
                                         *width* *height*
                                         (kaleidoscope-eye frame)
                                         (kaleidoscope-mirrors frame)
                                         (kaleidoscope-mirrors-polygon frame)))
            (redisplay-frame-pane frame pane :force-p t)))))))

(define-presentation-to-command-translator change-mirror
    (blank-area com-draw-mirror kaleidoscope
     :gesture :select
     :echo nil
     :tester ((object)
              (declare (ignore object))
              (let ((frame *application-frame*))
                (eq (pointer-sheet (port-pointer (port frame)))
                    (get-frame-pane frame 'canvas)))))
    (object x y)
  (list x y))

(define-presentation-to-command-translator move-eye
    (eye com-move-eye kaleidoscope
     :gesture :select
     :echo t)
    (object)
  (list object))

(define-drag-and-drop-translator tr-move-eye
    (eye command mirrors kaleidoscope
         :feedback (lambda (frame presentation stream x0 y0 x1 y1 state)
                     (declare (ignore frame))
                     (case state
                       (:highlight
                        (when (> (sqrt (+ (expt (- x1 x0) 2) (expt (- y1 y0) 2))) 4)
                          (let ((eye (presentation-object presentation)))
                            (with-output-recording-options (stream :draw t :record nil)
                              (with-drawing-options
                                  (stream :transformation
                                          (make-translation-transformation (- x1 (point-x eye))
                                                                           (- y1 (point-y eye))))
                                (draw eye stream))))))
                       (:unhighlight
                        (repaint-sheet stream (make-rectangle*
                                               (- x1 32)
                                               (- y1 32)
                                               (+ x1 32)
                                               (+ y1 32))))))
         :destination-tester
         ((object destination-object)
          (let ((pos (get-pointer-position (find-pane-named *application-frame* 'canvas))))
            (region-contains-position-p destination-object (point-x pos) (point-y pos))))
         :multiple-window nil
         :menu nil)
    (object)
  (let ((position (get-pointer-position (find-pane-named *application-frame* 'canvas))))
    `(com-move-eye ,position)))

(define-kaleidoscope-command (com-draw-mirror :name "Draw mirror") ((x 'real) (y 'real))
  (draw-mirror (find-pane-named *application-frame* 'canvas) x y))

(define-kaleidoscope-command (com-move-eye :name "Move eye") ((position))
  (let ((frame *application-frame*)
        (progress-frame (find-application-frame 'reflections-progress)))
    (setf (kaleidoscope-eye frame) (make-eye (point-x position) (point-y position))
          (kaleidoscope-reflections frame) (calculate-reflections progress-frame
                                                                  (kaleidoscope-reflections frame)
                                                                  *width* *height*
                                                                  (kaleidoscope-eye frame)
                                                                  (kaleidoscope-mirrors frame)
                                                                  (kaleidoscope-mirrors-polygon frame)))
    (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'canvas)
                          :force-p t)))

(defun get-pointer-position (pane)
  (multiple-value-bind (x y) (stream-pointer-position pane)
    (make-point x y)))

(defun start ()
  (let* ((mirrors (list (make-mirror (make-point 464 560) (make-point 560 560))
                        (make-mirror (make-point 464 560) (make-point 464 464))
                        (make-mirror (make-point 560 560) (make-point 464 464))))
         (point (make-point 560 575))
         (eye (make-eye 500 524))
         (polygon (make-mirrors mirrors))
         (reflections (make-array (list (1+ *width*) (1+ *height*))
                                  :initial-element nil))
         (reflections-progress (find-application-frame 'reflections-progress)))
    (find-application-frame 'kaleidoscope
                            :mirrors mirrors
                            :mirrors-polygon polygon
                            :eye eye
                            :point point
                            :reflections (calculate-reflections reflections-progress
                                                                reflections
                                                                *width* *height*
                                                                eye mirrors polygon))))
