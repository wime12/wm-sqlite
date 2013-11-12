(in-package #:wm-sqlite)

(defclass object-caching-mixin ()
  ((object-cache :reader object-cache
		:initform (make-hash-table :test 'equal))))

(defgeneric update-records-from-cache (database)
  (:method ((database object-caching-mixin))
    (with-transaction (database)
      (maphash-values
       (lambda (instance)
	 (update-record instance database))))))

(defmethod close-database :before ((database object-caching-mixin))
  (update-records-from-cache database))
