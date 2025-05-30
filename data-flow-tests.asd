;;;; data-flow-tests.asd
;;
;;;; Copyright (c) 2025 Tomáš Zellerin <tomas@zellerin.cz>


(asdf:defsystem #:data-flow-tests
  :description "Describe data-flow-tests here"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:mgl-pax #:clip-1994)
  :in-order-to ((test-op (test-op "data-flow-tests/test")))
  :components ((:file "package")
               (:file "transducers")))


(defsystem #:data-flow-tests/test
  :depends-on ("data-flow-tests" "fiasco")
  :perform (test-op (o s)
                    (symbol-call :fiasco '#:run-package-tests
                                 :package '#:data-flow-tests/tests
                                 :verbose asdf:*verbose-out*)
  :serial t)
  :license  "MIT"
  :pathname "tests"
  :components ((:file "package")
               (:file "transducers")))
