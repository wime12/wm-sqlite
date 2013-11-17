(in-package #:wm-sqlite)

(defclass autoincrement-mixin ()
  ())

(defun check-autoincrement (class)
  (let* ((primary-key (primary-key class))
	 (primary-key-length (length primary-key)))
    (when (zerop primary-key-length)
      (error "Cannot use autoincrement-mixin: no primary key defined for ~S."
	     class))
    (when (> primary-key-length 1)
      (error "Cannot use autoincrement-mixin: primary key of ~S does not consist of a single slot." class))
    (unless (eq (sqlite-persistent-slot-definition-persistence
		 (find-slotd class (car primary-key)))
		:integer)
      (error "Cannot use autoincrement-mixin: primary key slot of ~S is not declared to have type :INTEGER." class))
    (unless (eq (sqlite-persistent-slot-definition-primary-key
		 (find-slotd class (car (primary-key class))))
		:autoincrement)
      (error "Cannot use autoincrement-mixin: primary key slot of ~S is not declared as :autoincrement." class))))

(defmethod insert-record ((database database) (instance autoincrement-mixin))
  (call-next-method)
  (let ((id-slot (car (primary-key (class-of instance)))))
    (unless (slot-value instance id-slot)
      (setf (slot-value instance id-slot) (last-insert-rowid database))))
  instance)

