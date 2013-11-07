;;;; -*- coding: utf-8-unix -*-
;;;; wm-sqlite.asd.newest

(asdf:defsystem #:wm-sqlite
  :serial t
  :description "Describe wm-sqlite here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:cffi
	       #:trivial-garbage
	       #:trivial-gray-streams
	       #:closer-mop)
  :components ((:file "package")
	       (:file "ffi")
               (:file "sqlite")
	       (:file "template")
	       (:file "persistence")))

