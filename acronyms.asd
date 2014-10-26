(defpackage info.isoraqathedh.acronyms.asdf
  (:use #:cl #:asdf))
(in-package :info.isoraqathedh.acronyms.asdf)

(defsystem acronyms
  :name "Acronym Expander"
  :version "1.0.0"
  :licence "MIT"
  :description "A program that expands an acronym based on grammatical rules."
  :serial t
  :components ((:static-file "mobyposi.i")
               (:file "acronyms-2")))
