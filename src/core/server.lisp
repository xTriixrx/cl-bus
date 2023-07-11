;;;; server.lisp
;;;;
;;;; Server socket interface for receiving bus calls from clients.
;;;;
;;;; v0.0.1

(in-package :cl-bus)

(defvar *LOCALHOST* "127.0.0.1")

(defun insert-after (lst index element)
  (if (eql lst nil)
      (setf lst (cons element nil))
      (push element (cdr (nthcdr index lst)))) lst)

(defun handle-stream (socket)
  (bt::make-thread
   (lambda ()
     (let ((data nil)
           (stream (usocket::socket-stream socket)))
       (unwind-protect
            (loop
              (usocket::wait-for-input socket)
              (setf data (read-line stream))
              (format t "Received: ~a~%" data)
              (if (string-equal data "-q")
                  (progn
                    (format t "Received close message!~%")
                    (return))))
         (usocket::socket-close socket))))))

(defun make-server-thread (port)
  (bt::make-thread
   (lambda ()
     (let ((handlers nil))
       (usocket::with-socket-listener (server-socket "127.0.0.1" port :reuse-address t)
         (loop
           (let* ((socket (usocket::socket-accept server-socket)))
             (setf handlers (insert-after handlers (- (length handlers) 1) (handle-stream socket)))
             (setf handlers (remove-if-not #'bt::thread-alive-p handlers))
             (format t "Number of active handlers: ~a~%" (length handlers))
             (format t "Active: ~a~%" handlers))))))))

(defun start-server (port)
  (let ((serverThread (make-server-thread port)))
    (unwind-protect
      (progn
        (loop
          (if (not (bt::thread-alive-p serverThread)) (return) (sleep 1))))
      (bt::destroy-thread serverThread))))

(defun start (&optional (port 5555))
  (start-server port))
