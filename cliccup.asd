;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :co.fwoar.cliccup
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:fwoar-lisputils/string-utils
               #:plump)
  :serial t
  :components ((:file "cliccup")))
