;;;; dots-and-boxes.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:dots-and-boxes
  :description "Describe dots-and-boxes here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:qtools
               #:qtgui
               #:qtcore)
  :serial t
  :components ((:file "package")
               (:file "dots-and-boxes")))

