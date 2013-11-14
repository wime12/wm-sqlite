(in-package #:wm-sqlite)

(defclass object-caching-database-mixin ()
  ((object-cache :reader object-cache
		 :initform (make-hash-table :test 'equal))))

(defgeneric update-records-from-cache (database)
  (:method ((database object-caching-database-mixin))
    (with-transaction (database)
      (maphash-values
       (lambda (instance)
	 (update-record instance database))
       (object-cache database)))))

#+nil(defmethod close-database :before ((database object-caching-database-mixin))
  (update-records-from-cache database))

(defun cached-object (values database)
  (gethash values (object-cache database)))

(defun (setf cached-object) (object values database)
  (setf (gethash values (object-cache database)) object)
  object)

(defmethod make-persistent-object
    ((database object-caching-database-mixin) (class symbol) values)
  (make-persistent-object database (find-class class) values))

(defmethod make-persistent-object
    ((database object-caching-database-mixin) (class sqlite-persistent-class) values)
  (let ((signature (cons (class-name class) values)))
    (or (cached-object signature database)
	(let ((object (call-next-method)))
	  (setf (cached-object (copy-list signature) database) object)
	  object))))

(defmethod insert-record :after ((database object-caching-database-mixin)
				 (instance sqlite-persistent-object))
  (let* ((class (class-of instance)))
    (setf (cached-object
	   (cons (class-name class)
		 (mapcar
		  (lambda (slot) (slot-value instance slot))
		  (second
		   (sqlite-persistent-class-insert-record-string class))))
	   (object-cache database))
	  instance)))

(defun clear-object-cache (&optional (database *default-database*))
  (clrhash (object-cache database))
  database)
