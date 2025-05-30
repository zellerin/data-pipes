;;;; package.lisp
;;
;;;; Copyright (c) 2025 Tomáš Zellerin <tomas@zellerin.cz>


(mgl-pax:define-package #:data-flow-tests
  (:use #:cl #:tz-utilities #:mgl-pax #:clip)
  (:documentation "TODO: document me"))

(in-package data-flow-tests)

(defsection @overview
    (:title "overview")
  ;; TODO: add overview
  )

(defsection @index
    (:title "overview")
  (@overview section)
  ;; TODO: add other sections
  ( data-flow-tests asdf/system:system)
)
