(in-package #:wm-sqlite)

(defclass statement-caching-mixin ()
  ((statement-cache :initform (make-hash-table :test 'equal)
	  :accessor statement-cache)))

(defgeneric clear-statement-cache (database)
  (:method ((database (eql t)))
    (clear-statement-cache *default-database*))
  (:method ((database statement-caching-mixin))
    (clrhash (statement-cache database))))

(defmethod prepare ((instance statement-caching-mixin) sql
		    &optional (length -1))
  (declare (ignore length))
  (let* ((cache (statement-cache instance))
	 (cached-statement (gethash sql cache)))
    (if cached-statement
	(values (car cached-statement) (cdr cached-statement))
	(multiple-value-bind (statement tail) (call-next-method)
	  (setf (gethash sql cache) (cons statement tail))
	  (values statement tail)))))
