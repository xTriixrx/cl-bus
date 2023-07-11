;;;; bus-client.lisp
;;;;
;;;; Client socket interface for sending bus calls to the cl-bus middleware.
;;;;
;;;; v0.0.1

(defpackage :cl-bus-client
  (:use :cl :usocket :bordeaux-threads))

(in-package :cl-bus-client)

;; connect

(defun create-client (&optional (port 5555))
  (usocket::with-client-socket (socket stream "127.0.0.1" port)
    (unwind-protect
         (progn
           (let ((data nil))
             (loop
               (setf data (read-line))
               (if (string-equal data "-q")
                   (progn
                     (format stream "-q~%")
                     (force-output stream)
                     (return)))
             (format stream "~a~%" data)
             (force-output stream))))
      (usocket::socket-close socket))))

;; subscribe

;; request

;; reply
