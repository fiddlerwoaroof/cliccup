;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :co.fwoar.cliccup
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:plump)
  :serial t
  :components ((:file "cliccup")))
