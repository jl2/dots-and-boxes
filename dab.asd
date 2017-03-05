;;;; dab.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:dab
  :description "A Dots and Boxes game in Common Lisp and Qt."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (#:qtools
               #:qtgui
               #:qtcore)
  :serial t
  :components ((:file "package")
               (:file "dab")))

