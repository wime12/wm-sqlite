(in-package #:wm-sqlite)

(defclass object-caching-mixin ()
  ((object-cache :reader object-cache
		:initform (make-hash-table :test 'equal))))
