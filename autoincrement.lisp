(in-package #:wm-sqlite)

(defclass autoincrement-mixin ()
  ())

(defun check-autoincrement (class)
  (let* ((primary-key (primary-key class))
	 (primary-key-length (length primary-key)))
    (when (zerop primary-key-length)
      (error "No primary key defined for ~S: cannot use autoincrement-mixin."
	     class))
    (when (> primary-key-length 1)
      (error "Primary key of ~S does not consist of a single slot: Cannot use autoincrement-mixin." class))
    (unless (eq (sqlite-persistent-slot-definition-persistence
		 (find-slotd class (car primary-key)))
		:integer)
      (error "Primary key slot of ~S is not declared to have type :INTEGER: Cannot use autoincrement-mixin." class))))

(defmethod insert-record ((database database) (instance autoincrement-mixin))
  (call-next-method)
  (let ((id-slot (car (primary-key (class-of instance)))))
    (unless (slot-value instance id-slot)
      (setf (slot-value instance id-slot) (last-insert-rowid database))))
  instance)

