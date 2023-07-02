;;;; ring.lisp
;;;;
;;;; A persistent circular ring buffer implementation. Values will persist
;;;; within buffer until value has been read; meaning no values are
;;;; overwritten prior to reading. This implementation is not thread safe
;;;; and must be wrapped when an instance is use by multiple threads. The
;;;; underlying data structure is a simple array type.
;;;;
;;;; v0.0.1

(defpackage :rb
  (:use :cl))

(in-package :rb) ;; rb = ring buffer

(defvar *rb-default-capacity* 1024)

;; Class definition for ring buffer
(defclass ring-buffer ()
  ((capacity :initarg :capacity) ;; size of buffer
   (buffer :initarg :buffer) ;; raw buffer
   (head :initform 0) ;; head index
   (tail :initform 0) ;; tail index
   (full-p :initform nil))) ;; full predicate

;; Constructor for generating ring buffer, by default ring allocates 1024 elements
(defun make-rb (&optional capacity)
  ;; If a capacity is given, create buffer to that size
  (if (and (numberp capacity) (> capacity 0))
    (make-instance 'ring-buffer
     :capacity capacity
     :buffer (make-array capacity :initial-element nil))
    (make-instance 'ring-buffer
     :capacity *rb-default-capacity*
     :buffer (make-array *rb-default-capacity* :initial-element nil))))

;; Class method for writing data to ring buffer
(defmethod rb-write ((ring ring-buffer) data)
  (with-slots (buffer capacity head tail full-p) ring
    ;; If ring buffer is not full, insert new data
    (if (not (rb-full-p ring))
      (progn
        (setf (aref buffer head) data)
        ;; Update head index
        (setf head (mod (+ 1 head) capacity))
        ;; If head catches back up with tail, buffer is full
        (if (eql head tail) (setf full-p t)) t) nil)))

;; Class method for reading data from ring buffer
(defmethod rb-read ((ring ring-buffer))
  (let ((data nil))
    (with-slots (buffer capacity head tail full-p) ring
     ;; If ring buffer is not empty
     (if (not (rb-empty-p ring))
      (progn
       (setf data (aref buffer tail))
       ;; If head and tail are equal, reset full-p flag
       (if (eql head tail) (setf full-p nil))
       ;; Update tail index
       (setf tail (mod (+ 1 tail) capacity)))))
    data))

(defmethod rb-empty-p ((ring ring-buffer))
  (with-slots (head tail) ring
    (and (eql head tail) (not (rb-full-p ring)))))

(defmethod rb-full-p ((ring ring-buffer))
  (with-slots (head tail full-p) ring
    (and (eql head tail) full-p)))

(defmethod rb-reset ((ring ring-buffer))
  (with-slots (buffer capacity head tail full-p) ring
    (setf buffer (make-array capacity :initial-element nil))
    (setf head 0)
    (setf tail 0)
    (setf full-p nil)) t)

;; Class method for printing slots from ring buffer class instance
(defmethod rb-print ((ring ring-buffer))
  (with-slots (buffer head tail) ring
    (format t "Buffer: ~a~%" buffer)
    (format t "Head Index: ~a~%" head)
    (format t "Tail Index: ~a~%" tail)))
