(defsystem "cl-bus"
  :version "0.1.0"
  :author "Vincent Nigro"
  :license ""
  :depends-on ("usocket" "bordeaux-threads")
  :components ((:module "src/ring"
                        :components ((:file "ring")))
               (:module "src/core"
                        :components ((:file "bus")
                                     (:file "server")))
               (:module "src/client"
                        :components ((:file "bus-client"))))
  :description "A Common Lisp message bus for message passing between decoupled applications."
  :in-order-to ((test-op (test-op "cl-bus/tests"))))

(defsystem "cl-bus/tests"
  :author "Vincent Nigro"
  :license ""
  :depends-on ("cl-bus" "rove")
  :components ((:module "tests"
                        :components ((:file "test-ring")
                                     (:file "test-bus")
                                     (:file "test-server")
                                     (:file "test-client"))))
  :description "Test system for cl-bus"
  :perform (test-op (op c) (symbol-call :rove :run c)))
