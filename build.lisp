(require "asdf")

(ql:quickload :usocket)
(ql:quickload :bordeaux-threads)

(asdf:load-asd "cl-bus.asd")
(asdf:load-system :cl-bus)
(in-package :cl-bus)

(sb-ext:save-lisp-and-die "bus" :toplevel 'start :executable t)
(sb-ext:exit)
