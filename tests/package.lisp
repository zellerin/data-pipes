(fiasco:define-test-package #:data-flow-tests/tests
  (:use #:cl #:fiasco #:data-flow-tests))

(in-package data-flow-tests/tests)

(deftest dummy-test ()
  "Test that testing works"
  (is t))
