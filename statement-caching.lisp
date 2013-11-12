(in-package #:wm-sqlite)

(defclass statement-caching-mixin ()
  ((cache :initform (make-hash-table :test 'equal)
	  :accessor statement-cache)))

(defmethod prepare ((instance statement-caching-mixin) sql
		    &optional (length -1))
  (declare (ignore length))
  (let ((cache (statement-cache instance)))
    (or (gethash sql cache)
	(setf (gethash sql cache) (call-next-method)))))
