;;;; kaleidoscope.lisp

(in-package #:kaleidoscope)

(defparameter *threads* 16)
(defparameter *width* 1024)
(defparameter *height* 1024)
(defparameter *style* (make-text-style :serif :roman 24))

(setf lparallel:*kernel* (lparallel:make-kernel *threads*))

(defun mirrors-polygon (mirrors)
  (make-polygon (append (loop for m in mirrors
                              collect (line-start-point m))
                        (list (line-end-point (first (last mirrors)))))))

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

(defun calculate-reflections (frame width height eye mirrors polygon)
  (let* ((reflections (make-array (list (1+ width) (1+ height))
                                  :initial-element nil))
         (div 8)
         (delta (/ (/ *height* div))))
    (lparallel:pmapcar (lambda (coord)
                         (let* ((i (first coord))
                                (j (second coord)))
                           (setf (aref reflections i j)
                                 (calculate-reflection i j eye mirrors polygon))
                           (when(and (= j height) (zerop (mod i div)))
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

(defun draw-mirrors (frame canvas)
  (loop for mirror in (kaleidoscope-mirrors frame)
        do (draw mirror canvas)))

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
        (draw-mirrors frame canvas)))))

(defun display-canvas (frame pane)
  (window-clear pane)
  (draw-all frame pane))

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
  ((mirrors :initarg :mirrors :reader kaleidoscope-mirrors)
   (mirrors-polygon :initarg :mirrors-polygon :reader kaleidoscope-mirrors-polygon)
   (kaleidoscope-reflections :initarg :reflections :reader kaleidoscope-reflections)
   (eye :initarg :eye :reader kaleidoscope-eye)
   (point :initarg :point :accessor kaleidoscope-point))
  (:panes (canvas (make-pane 'application-pane
                             :name 'canvas
                             :background +white+
                             :display-function #'display-canvas
                             :display-time :command-loop))
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
  t)

(define-kaleidoscope-command (com-select-point :name "Select point")
    ((point 'point :prompt "point"))
  (setf (kaleidoscope-point *application-frame*) point)
  (format (get-frame-pane *application-frame* 'interactor) "~D ~D~%"
          (round (point-x point)) (round (point-y point))))

(defun get-pointer-position (pane)
  (multiple-value-bind (x y) (stream-pointer-position pane)
    (make-point x y)))

(define-presentation-to-command-translator translator-select-point
    (blank-area com-select-point kaleidoscope
                :documentation "Select point"
                :tester
                ((object)
                 (let ((frame *application-frame*))
                   (eq (pointer-sheet (port-pointer (port frame)))
                       (get-frame-pane frame 'canvas)))))
    (object)
  (list (get-pointer-position
            (get-frame-pane *application-frame* 'canvas))))

(defun start ()
  (let* ((mirrors
           (list (make-mirror (make-point 464 520)
                              (make-point 560 520))
                 (make-mirror (make-point 464 520)
                              (make-point 464 464))
                 (make-mirror (make-point 560 520)
                              (make-point 464 464))))
         (reflections-progress (find-application-frame 'reflections-progress))
         (point (make-point 540 470))
         (eye (make-eye 480 500))
         (polygon (mirrors-polygon mirrors)))
    (find-application-frame 'kaleidoscope
                            :mirrors mirrors
                            :mirrors-polygon polygon
                            :eye eye
                            :point point
                            :reflections (calculate-reflections reflections-progress
                                                                *width* *height*
                                                                eye mirrors polygon))))

#|(defun draw-all (frame canvas)
(draw-mirrors frame canvas)
(draw (kaleidoscope-eye frame) canvas)
(draw-reflection frame canvas))|#
#|(list (make-mirror (make-point 464 560) (make-point 560 560))
(make-mirror (make-point 464 560) (make-point 464 464))
(make-mirror (make-point 560 560) (make-point 464 464)))|#
#|(list (make-mirror (make-point 414 575)
(make-point 560 575))
(make-mirror (make-point 414 575)
(make-point 380 442))
(make-mirror (make-point 560 575)
(make-point 380 442)))|#
#|(eye (make-eye 500 524))|#
