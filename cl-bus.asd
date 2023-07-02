(defsystem "cl-bus"
  :version "0.1.0"
  :author "Vincent Nigro"
  :license ""
  :components ((:module "src/ring"
                        :components ((:file "ring"))))
  :description "A Common Lisp message bus for message passing between decoupled applications."
  :in-order-to ((test-op (test-op "cl-bus/tests"))))

(defsystem "cl-bus/tests"
  :author "Vincent Nigro"
  :license ""
  :depends-on ("cl-bus" "rove")
  :components ((:module "tests"
                        :components ((:file "test-ring"))))
  :description "Test system for cl-bus"
  :perform (test-op (op c) (symbol-call :rove :run c)))
