(in-package #:wm-sqlite)

;;; Metaclass

(defclass sqlite-caching-persistent-class (sqlite-persistent-class)
  ((object-cache
    :reader object-cache
    :initform (make-hash-table :test 'equal))
   (signature-cache
    :reader signature-cache
    :initform (make-hash-table :test 'eq))
   (signature-indices
    :reader signature-indices)))

(defmethod finalize-inheritance :after ((class sqlite-caching-persistent-class))
  (let ((primary-key (sqlite-persistent-class-primary-key class)))
    (when (null primary-key)
      (error "Cannot implement caching for ~S: It does not have a primary key."
	     class))
    (setf (slot-value class 'signature-indices)
	  (compute-signature-indices class))))

(defun compute-signature-indices (class)
  (let ((pslots (sqlite-persistent-class-persistent-slots class)))
    (mapcar (lambda (primary-key-slot)
	      (position primary-key-slot pslots))
	    (sqlite-persistent-class-primary-key class))))

(defun signature-from-instance (instance)
  (mapcar (lambda (slot)
	    (slot-value instance slot))
	  (sqlite-persistent-class-primary-key (class-of instance))))

(defun signature-from-values (values class)
  (mapcar (lambda (i) (svref values i)) (signature-indices class)))

(defun cached-object (signature persistent-class)
  (gethash signature (object-cache persistent-class)))

(defun (setf cached-object) (object signature persistent-class)
  (setf (gethash signature (object-cache persistent-class)) object)
  (setf (gethash object (signature-cache persistent-class)) signature)
  object)

(defmethod make-persistent-instance ((class sqlite-caching-persistent-class)
				     values)
  (let* ((signature (signature-from-values values class)))
    (or (cached-object signature class)
	(setf (cached-object signature class) (call-next-method)))))

(defclass sqlite-caching-persistent-object (sqlite-persistent-object)
  ())

(defmethod initialize-instance :around ((class sqlite-caching-persistent-class)
					&rest initargs
					&key direct-superclasses)
  (let ((ocm (find-class 'sqlite-caching-persistent-object))
	(scpc (find-class 'sqlite-caching-persistent-class)))
    (if (member-if
	 (lambda (super)
	   (eq (class-of super) scpc))
	 direct-superclasses)
	(call-next-method)
	(apply #'call-next-method
	       class
	       :direct-superclasses (append direct-superclasses (list ocm))
	       initargs))))

(defmethod reinitialize-instance :around
    ((class sqlite-caching-persistent-class) &rest initargs
     &key (direct-superclasses nil direct-superclasses-p))
  (let ((ocm (find-class 'sqlite-caching-persistent-object)))
    (prog1
	(if direct-superclasses-p
	    (let ((scpc (find-class 'sqlite-caching-persistent-class)))
	      (if (or
		   (eq class ocm)
		   (member-if
		    (lambda (super)
		      (eq (class-of super) scpc))
		    direct-superclasses))
		  (call-next-method)
		  (apply #'call-next-method
			 class
			 :direct-superclasses (append direct-superclasses
						      (list ocm))
			 initargs)))
	    (call-next-method)))))


;;;

(defmethod insert-record :after ((database database)
				 (instance sqlite-caching-persistent-object))
  (setf (cached-object
	 (signature-from-instance instance)
	 (class-of instance))
	instance))

;;; TODO: control slot-access to primary key slots in order to keep caching consistent or just alert the user never to change primary keys

(defmethod delete-record :after ((database database)
				 (instance sqlite-caching-persistent-object))
  (let ((class (class-of instance)))
    (remhash (signature-from-instance instance) (object-cache class))
    (remhash instance (signature-cache class))))

(defgeneric clear-object-cache (class)
  (:method ((class symbol))
    (clear-object-cache (find-class class)))
  (:method ((class sqlite-caching-persistent-class))
    (clrhash (object-cache class))
    (clrhash (signature-cache class))
    nil))
