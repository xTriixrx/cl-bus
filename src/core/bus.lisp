;;;; bus.lisp
;;;;
;;;; Test
;;;;
;;;; v0.0.1

(defpackage :cl-bus
  (:use :cl :rb :usocket :bordeaux-threads))

(in-package :cl-bus)

(defclass cl-bus ()
  ((buffer :initarg rb::ring-buffer)))

(defun make-cl-bus (&optional capacity)
  (if (and (numberp capacity) (> capacity 0))
    (make-instance 'cl-bus :buffer (rb::make-rb capacity))
    (make-instance 'cl-bus :buffer (rb::make-rb))))

(defmethod publish ((bus cl-bus) msg)
  (with-slots (buffer) bus
    (rb::rb-write buffer msg)))

(defmethod read-bus ((bus cl-bus))
  (with-slots (buffer) bus
    (rb::rb-read buffer)))
