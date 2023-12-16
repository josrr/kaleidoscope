;;;; objects.lisp

(in-package #:kaleidoscope)

(defclass mirror (standard-line)
  ((normal :initarg :normal :initform nil :reader mirror-normal)))

(defun make-mirror (p1 p2)
  (make-instance 'mirror
                 :x1 (point-x p1) :y1 (point-y p1)
                 :x2 (point-x p2) :y2 (point-y p2)
                 :normal (vunit (vec (- (point-y p1) (point-y p2))
                                     (- (point-x p2) (point-x p1))))))

(defclass eye (standard-point)
  ((vector :initarg :vector :initform nil :reader eye-vector)))

(defun make-eye (x y)
  (make-instance 'eye :x x :y y :vector (vec x y)))

(defmethod draw ((object mirror) stream)
  (draw-design stream object :line-thickness 4 :ink +flipping-ink+))

(defmethod draw ((object eye) stream)
  (draw-design stream object :line-thickness 20 :ink +flipping-ink+)
  (draw-design stream object :line-thickness 4 :ink +black+ :filled nil))

(defclass ray (standard-line)
  ((vec-start :initarg :vec-start :initform nil :reader ray-vec-start)
   (vec-end :initarg :vec-end :initform nil :reader ray-vec-end)))

(defun make-ray (eye point)
  (make-instance 'ray
                 :x1 (point-x eye) :y1 (point-y eye)
                 :x2 (point-x point) :y2 (point-y point)
                 :vec-start (vec (point-x eye) (point-y eye))
                 :vec-end (vec (point-x point) (point-y point))))

(defmethod draw ((object ray) stream)
  ;;(draw-design stream object :line-thickness 2 :ink +red+)
  (draw-line* stream
              (vx (ray-vec-start object)) (vy (ray-vec-start object))
              (vx (ray-vec-end object)) (vy (ray-vec-end object))
              :line-thickness 2 :ink +red+)
  (draw-point* stream
               (vx (ray-vec-end object)) (vy (ray-vec-end object))
               :line-thickness 10 :ink +red+))

(defclass reflection ()
  ((point :initarg :point :initform nil :reader reflection-point)
   (times :initarg :times :initform nil :reader reflection-times)))

(defun make-reflection (ray mirrors times)
  (multiple-value-bind (in mirror) (loop for mirror in mirrors
                                         for in = (region-intersection mirror ray)
                                         unless (eq +nowhere+ in) return (values in mirror))
    (if in
        (let* ((vin (vec (point-x in) (point-y in)))
               (ray/2 (nv- vin (ray-vec-end ray)))
               (dot (v. (nv- ray/2) (mirror-normal mirror)))
               (2*dot-normal (v* (mirror-normal mirror) dot 2.0))
               (vp (nv- 2*dot-normal (ray-vec-end ray))))
          (make-instance 'reflection
                         :point (make-point (- (vx2 vp))
                                            (- (vy2 vp)))
                         ;;:intersection in
                         :times times))
        nil)))

#|
(defmethod draw ((object reflection) stream)
  (draw-line* stream
              (point-x (reflection-intersection object)) (point-y (reflection-intersection object))
              (point-x (reflection-point object)) (point-y (reflection-point object))
              :line-thickness 2
              :ink +green+)
  (draw-design stream (reflection-point object)
               :line-thickness 10
               :ink +blue+))
(intersection :initarg :intersection :initform nil :reader reflection-intersection)
|#
