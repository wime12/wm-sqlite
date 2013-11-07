;;;; -*- coding: utf-8-unix -*-

(in-package #:wm-sqlite)

;;; Utilities

(defun sql-identifier (str)
  (substitute #\_ #\- (string-downcase str)))

;;; Persistent Class

(defclass sqlite-persistent-class (standard-class)
  ((insert-record-string
    :reader sqlite-persistent-class-insert-record-string
    :initform nil)
   (update-record-string
    :reader sqlite-persistent-class-update-record-string
    :initform nil)
   (delete-record-string
    :reader sqlite-persistent-class-delete-record-string
    :initform nil)
   (update-from-record-string
    :reader sqlite-persistent-class-update-from-record-string
    :initform nil)
   (select-string
    :reader sqlite-persistent-class-select-string
    :initform nil)
   (table-name
    :initarg :table-name
    :reader sqlite-persistent-class-table-name
    :initform nil)
   (primary-key
    :initarg :primary-key
    :reader sqlite-persistent-class-primary-key)
   (foreign-keys
    :initarg :foreign-keys
    :reader sqlite-persistent-class-foreign-keys
    :initform nil)
   (unique-constraints
    :initarg :unique
    :reader sqlite-persistent-class-unique-constraints
    :initform nil)))

(defun initialize-persistent-class-slots (class)
  (with-slots (table-name primary-key foreign-keys) class
    (setf table-name (or (and (listp table-name) (car table-name))
			 table-name
			 (compute-table-name class))
	  primary-key (compute-primary-key class)
	  foreign-keys (mapcar #'normalize-foreign-key-spec foreign-keys)))
  (set-sql-strings class))

(defun compute-primary-key (class)
  (mapcan
   (lambda (slotd)
     (when (sqlite-persistent-slot-definition-primary-key slotd)
       (list (slot-definition-name slotd))))
   (class-slots class)))

(defun normalize-foreign-key-spec (foreign-key-spec)
  (destructuring-bind (reference-table slots reference-slots &rest rest)
      foreign-key-spec
    (cons reference-table
	  (cons (mklist slots)
		(cons (mklist reference-slots)
		      rest)))))

(defun mklist (x)
  (if (listp x)
      x
      (list x)))

(defun set-sql-strings (class)
  (with-slots (insert-record-string
	       update-record-string
	       delete-record-string
	       update-from-record-string
	       select-string)
      class
    (setf insert-record-string (compute-insert-record-string class)
	  update-record-string (compute-update-record-string class)
	  delete-record-string (compute-delete-record-string class)
	  update-from-record-string (compute-update-from-record-string class)
	  select-string (compute-select-string class))))

(defmethod finalize-inheritance :after ((class sqlite-persistent-class))
  (unless (eq (class-name class) 'sqlite-persistent-object)
    (initialize-persistent-class-slots class)))

(defmethod validate-superclass ((class sqlite-persistent-class)
				(superclass standard-class))
  t)

(defun compute-table-name (class)
  (sql-identifier (symbol-name (class-name class))))

(defun persistent-slotds (slotds)
  (remove-if-not #'sqlite-persistent-slot-definition-persistence slotds))

(defun primary-key-slotds (slotds)
  (remove-if-not #'sqlite-persistent-slot-definition-primary-key slotds))

(defun non-primary-key-slotds (slotds)
  (remove-if #'sqlite-persistent-slot-definition-primary-key slotds))

(defun multi-primary-key-p (slotds)
  (> (count-if
      (lambda (slotd)
	(not (null (sqlite-persistent-slot-definition-primary-key slotd))))
      slotds)
     1))

(defun slot-name-sql-identifier (slotd)
  (sql-identifier (slot-definition-name slotd)))

(defun compute-insert-record-string (class)
  (let ((pslotds (persistent-slotds (class-slots class))))
    (list
     (format nil "insert into ~A(~{~A~^, ~}) values(~{@~A~^, ~});"
	    (sqlite-persistent-class-table-name class)
	    (mapcar #'sqlite-persistent-slot-definition-column-name pslotds)
	    (mapcar #'slot-name-sql-identifier pslotds))
     (mapcar #'slot-definition-name pslotds))))

(defun compute-update-record-string (class)
  (let ((pkslotds (primary-key-slotds (class-slots class)))
	(npkslotds (non-primary-key-slotds (class-slots class))))
    (when pkslotds
      (list
       (format nil "update ~A set ~{~A = @~A~^, ~} where ~{(~A = @~A)~^ and ~};"
	       (sqlite-persistent-class-table-name class)
	       (mapcan #'list
		       (mapcar #'sqlite-persistent-slot-definition-column-name
			       npkslotds)
		       (mapcar #'slot-name-sql-identifier npkslotds))
	       (mapcan #'list
		       (mapcar #'sqlite-persistent-slot-definition-column-name
			       pkslotds)
		       (mapcar #'slot-name-sql-identifier pkslotds)))
       (nconc (mapcar #'slot-definition-name npkslotds)
	      (mapcar #'slot-definition-name pkslotds))))))

(defun compute-delete-record-string (class)
  (let ((pkslotds (primary-key-slotds (class-slots class))))
    (when pkslotds
      (list
       (format nil "delete from ~A where ~{(~A = @~A)~^ and ~};"
	       (sqlite-persistent-class-table-name class)
	       (mapcan #'list
		       (mapcar #'sqlite-persistent-slot-definition-column-name
			       pkslotds)
		       (mapcar #'slot-name-sql-identifier pkslotds)))
       (mapcar #'slot-definition-name pkslotds)))))

(defun compute-update-from-record-string (class)
  (let ((npkslotds (non-primary-key-slotds (class-slots class)))
	(pkslotds (primary-key-slotds (class-slots class))))
    (when pkslotds
      (list
       (format nil "select ~{~A~^, ~} from ~A where ~{(~A = @~A)~^ and ~};"
	       (mapcar #'sqlite-persistent-slot-definition-column-name
		       npkslotds)
	       (sqlite-persistent-class-table-name class)
	       (mapcan #'list
		       (mapcar #'sqlite-persistent-slot-definition-column-name
			       pkslotds)
		       (mapcar #'slot-name-sql-identifier pkslotds)))
       (mapcar #'slot-definition-name npkslotds)
       (mapcar #'slot-definition-name pkslotds)))))

(defun find-slotd (class slot)
  (flet ((find-slotd-class (class slot-name)
	   (find slot-name
		 (the list (class-slots (ensure-finalized class)))
		 :key #'slot-definition-name
		 :test #'eq)))
    (etypecase class
      (symbol (find-slotd-class (find-class class) slot))
      (class (find-slotd-class class slot)))))

(defun compute-select-string (class)
  (let* ((pslotds (persistent-slotds (class-slots class))))
    (list
     (format nil "select ~{~A~^, ~} from ~A "
	     (mapcar #'sqlite-persistent-slot-definition-column-name pslotds)
	     (sqlite-persistent-class-table-name class))
     (mapcar #'slot-definition-name pslotds))))

(defun compute-schema-string (class)
  (let* ((multi-primary-key-p (> (length (the list (primary-key class))) 1)))
    (with-output-to-string (s)
      (format s "create table ~A (~%    "
	      (sqlite-persistent-class-table-name class))
      (attribute-lines class multi-primary-key-p s)
      (multi-primary-key-line class multi-primary-key-p s)
      (unique-constraint-lines class s)
      (foreign-key-lines class s)
      (princ ")" s))))

(defun attribute-lines (class multi-primary-key-p stream)
  (declare (stream stream))
  (let ((persistent-slot-definitions (persistent-slotds (class-slots class))))
    (maplist
     (lambda (slotds)
       (let ((slotd (first slotds)))
	 (princ (sqlite-persistent-slot-definition-column-name slotd) stream)
	 (let ((persistence
		(sqlite-persistent-slot-definition-persistence slotd)))
	   (unless (eq persistence t)
	     (princ " " stream)
	     (princ (sql-identifier (symbol-name persistence)) stream)))
	 (unless multi-primary-key-p
	   (let ((primary-key
		  (sqlite-persistent-slot-definition-primary-key slotd)))
	     (when primary-key
	       (princ " primary key" stream)
	       (when (eq primary-key :autoincrement)
		 (princ " autoincrement" stream)))))
	 (when (sqlite-persistent-slot-definition-not-null slotd)
	   (princ " not null" stream))
	 (when (sqlite-persistent-slot-definition-unique slotd)
	   (princ " unique" stream))
	 (when (cdr slotds)
	   (format stream ",~%    "))))
     persistent-slot-definitions)))

(defun multi-primary-key-line (class multi-primary-key-p stream)
  (declare (stream stream))
  (when multi-primary-key-p
    (format stream ",~%  primary key (~{~A~^, ~})"
	    (mapcar #'(lambda (slot-name)
			(slot-column-name class slot-name))
		    (primary-key class)))))

(defun unique-constraint-lines (class stream)
  (declare (stream stream))
  (mapc
   (lambda (constraint)
     (format stream ",~%  unique (~{~A~^, ~})"
	     (mapcar (lambda (slot)
		       (slot-column-name class slot))
		     constraint)))
   (sqlite-persistent-class-unique-constraints class)))

(defun foreign-key-lines (class stream)
  (declare (stream stream))
  (flet ((on-string (symbol)
	   (ecase symbol
	     (:set-null "set null")
	     (:set-default "set default")
	     (:cascade "cascade")
	     (:restrict "restrict")
	     (:no-action "no action"))))
    (let ((foreign-keys (foreign-keys class)))
      (when foreign-keys
	(mapc
	 (lambda (foreign-key)
	   (destructuring-bind (reference-table
				slot-names
				reference-slot-names
				&key on-delete on-update deferred)
	       foreign-key
	     (format stream
		     ",~%  foreign key (~{~A~^, ~}) references ~A(~{~A~^, ~})"
		     (mapcar (lambda (slot-name)
			       (slot-column-name class slot-name))
			     slot-names)
		     (table-name reference-table)
		     (mapcar (lambda (slot-name)
			       (slot-column-name reference-table slot-name))
			     reference-slot-names))
	     (when on-delete
	       (format stream " on delete ~A" (on-string on-delete)))
	     (when on-update
	       (format stream " on update ~A" (on-string on-update)))
	     (when deferred
	       (format stream " deferrable initially deferred"))))
	 foreign-keys)))))

;;; Direct Slots

(defclass sqlite-persistent-direct-slot-definition
    (standard-direct-slot-definition)
  ((persistence
    :reader sqlite-persistent-slot-definition-persistence
    :initarg :persistence
    :initform t)
   (primary-key
    :reader sqlite-persistent-slot-definition-primary-key
    :initarg :primary-key
    :initform nil)
   (not-null
    :reader sqlite-persistent-slot-definition-not-null
    :initarg :not-null
    :initform nil)
   (unique
    :reader sqlite-persistent-slot-definition-unique
    :initarg :unique
    :initform nil)
   (column-name
    :reader sqlite-persistent-slot-definition-column-name
    :initarg :column-name
    :initform nil)))


;; Integrate standard-slot-definitions

(defgeneric sqlite-persistent-slot-definition-persistence (slotd))
(defgeneric sqlite-persistent-slot-definition-primary-key (slotd))
(defgeneric sqlite-persistent-slot-definition-references (slotd))
(defgeneric sqlite-persistent-slot-definition-column-name (slotd))
(defgeneric sqlite-persistent-slot-definition-unique (slotd))
(defgeneric sqlite-persistent-slot-definition-not-null (slotd))

(defmethod sqlite-persistent-slot-definition-persistence
    ((slotd standard-direct-slot-definition))
  t)

(defmethod sqlite-persistent-slot-definition-primary-key
    ((slotd standard-direct-slot-definition))
  nil)

(defmethod sqlite-persistent-slot-definition-references
    ((slotd standard-direct-slot-definition))
  nil)

(defmethod sqlite-persistent-slot-definition-column-name
    ((slotd standard-direct-slot-definition))
  (sql-identifier (symbol-name (slot-definition-name slotd))))

(defmethod sqlite-persistent-slot-definition-unique
    ((slotd standard-direct-slot-definition))
  nil)

(defmethod sqlite-persistent-slot-definition-not-null
    ((slotd standard-direct-slot-definition))
  nil)

(defmethod initialize-instance :after
    ((slotd sqlite-persistent-direct-slot-definition)
     &rest initargs &key)
  (declare (ignore initargs))
  (if (null (sqlite-persistent-slot-definition-column-name slotd))
      (setf (slot-value slotd 'column-name)
	    (sql-identifier (symbol-name (slot-definition-name slotd)))))
  (let ((references (sqlite-persistent-slot-definition-references slotd)))
    (when (and references (symbolp (first references)))
      (setf (slot-value slotd 'references) (list references)))))

(defclass sqlite-persistent-effective-slot-definition
    (standard-effective-slot-definition)
  ((persistence
    :reader sqlite-persistent-slot-definition-persistence
    :initarg :persistence)
   (primary-key
    :reader sqlite-persistent-slot-definition-primary-key
    :initarg :primary-key)
   (not-null
    :reader sqlite-persistent-slot-definition-not-null
    :initarg :not-null)
   (unique
    :reader sqlite-persistent-slot-definition-unique
    :initarg :unique)
   (column-name
    :reader sqlite-persistent-slot-definition-column-name
    :initarg :column-name)))

(defmethod direct-slot-definition-class ((class sqlite-persistent-class)
					 &rest initargs)
  (declare (ignore initargs))
  (find-class 'sqlite-persistent-direct-slot-definition))

(defmethod effective-slot-definition-class ((class sqlite-persistent-class)
					    &rest initargs)
  (declare (ignore initargs))
  (find-class 'sqlite-persistent-effective-slot-definition))

(defmethod compute-effective-slot-definition :around
    ((class sqlite-persistent-class)
     name
     slotds)
  (declare (ignore name))
  (let ((slotd (call-next-method))
	(dslotd (first slotds)))
    (assert (not
	     (and (null (sqlite-persistent-slot-definition-persistence dslotd))
		  (or (sqlite-persistent-slot-definition-primary-key dslotd)
		      (sqlite-persistent-slot-definition-not-null dslotd)
		      (sqlite-persistent-slot-definition-unique dslotd)
		      (sqlite-persistent-slot-definition-column-name dslotd))))
	    ()
	    "In definition of class ~S: Wrong slot definition."
	    (class-name class))
    (with-slots (persistence primary-key not-null unique column-name)
	slotd
      (setf persistence (sqlite-persistent-slot-definition-persistence dslotd)
	    primary-key (sqlite-persistent-slot-definition-primary-key dslotd)
	    not-null (sqlite-persistent-slot-definition-not-null dslotd)
	    unique (sqlite-persistent-slot-definition-unique dslotd)
	    column-name (sqlite-persistent-slot-definition-column-name dslotd)))
    slotd))

;;; Generic Functions

(defgeneric table-name (persistent-class)
  (:method ((persistent-class symbol))
    (table-name (find-class persistent-class)))
  (:method ((persistent-class sqlite-persistent-class))
    (sqlite-persistent-class-table-name (ensure-finalized persistent-class))))

(defgeneric schema-string (persistent-class)
  (:method ((persistent-class symbol))
    (schema-string (find-class persistent-class)))
  (:method ((persistent-class sqlite-persistent-class))
    (compute-schema-string (ensure-finalized persistent-class))))

(defgeneric primary-key (persistent-class)
  (:method ((persistent-class symbol))
    (primary-key (find-class persistent-class)))
  (:method ((persistent-class sqlite-persistent-class))
    (sqlite-persistent-class-primary-key (ensure-finalized persistent-class))))

(defgeneric foreign-keys (persistent-class)
  (:method ((persistent-class symbol))
    (foreign-keys (find-class persistent-class)))
  (:method ((persistent-class sqlite-persistent-class))
    (sqlite-persistent-class-foreign-keys (ensure-finalized persistent-class))))

(defgeneric last-used-key (persistent-class &optional database)
  (:method ((class-name symbol) &optional (database *default-database*))
    (last-used-key (find-class class-name) database))
  (:method ((persistent-class sqlite-persistent-class)
	    &optional (database *default-database*))
    (with-open-query
	(q (bind-parameter
	    (prepare database "select seq from sqlite_sequence where name = ?")
	    1 (table-name persistent-class)))
      (read-row-column q 0))))

;;; Persistent Object

(defclass sqlite-persistent-object ()
  ()
  (:metaclass sqlite-persistent-class))

(defmethod initialize-instance :around ((class sqlite-persistent-class)
					&rest initargs
					&key direct-superclasses)
  (let ((spo (find-class 'sqlite-persistent-object))
	(spc (find-class 'sqlite-persistent-class)))
    (if (member-if
	 (lambda (super)
	   (eq (class-of super) spc))
	 direct-superclasses)
	(call-next-method)
	(apply #'call-next-method
	       class
	       :direct-superclasses (append direct-superclasses (list spo))
	       initargs))))

(defmethod reinitialize-instance :around
    ((class sqlite-persistent-class) &rest initargs
     &key (direct-superclasses nil direct-superclasses-p))
  (let ((spo (find-class 'sqlite-persistent-object)))
    (prog1
	(if direct-superclasses-p
	    (let ((spc (find-class 'sqlite-persistent-class)))
	      (if (or
		   (eq class spo)
		   (member-if
		    (lambda (super)
		      (eq (class-of super) spc))
		    direct-superclasses))
		  (call-next-method)
		  (apply #'call-next-method
			 class
			 :direct-superclasses (append direct-superclasses
						      (list spo))
			 initargs)))
	    (call-next-method)))))

(defgeneric update-record (persistent-object &optional database)
  (:method ((object sqlite-persistent-object)
	    &optional (database *default-database*))
    (let ((command
	   (sqlite-persistent-class-update-record-string (class-of object))))
      (assert command () "Update not possible for persisten class without primary key.")
      (destructuring-bind (statement-string in-slot-names) command
	(exec
	 (apply #'bind-parameters
		(prepare database statement-string)
		(slot-values object in-slot-names)))))
    object))

(defun slot-values (object slot-names)
  (mapcar
   (lambda (slot-name)
     (slot-value object slot-name))
   slot-names))

(defun set-slot-values (object slot-names values)
  (mapc
   (lambda (slot-name value)
     (setf (slot-value object slot-name) value))
   slot-names
   values)
  object)

(defgeneric update-from-record (persistent-object &optional database)
  (:method ((object sqlite-persistent-object)
	    &optional (database *default-database*))
    (let ((command
	   (sqlite-persistent-class-update-from-record-string
	    (class-of object))))
      (assert command () "Update from record not possible for persistent class without primary key.")
      (destructuring-bind (statement-string out-slot-names in-slot-names)
	  command
	(set-slot-values
	 object out-slot-names
	 (with-open-query (s (apply #'bind-parameters
				    (prepare database statement-string)
				    (slot-values object in-slot-names)))
	   (read-row s)))))
    object))

(defgeneric insert-record (persistent-object &optional database)
  (:method ((object sqlite-persistent-object)
	    &optional (database *default-database*))
    (destructuring-bind (statement-string in-slot-names)
	(sqlite-persistent-class-insert-record-string (class-of object))
      (exec (apply #'bind-parameters
		   (prepare database statement-string)
		   (slot-values object in-slot-names))))
    object))

(defgeneric delete-record (persistent-object &optional database)
  (:method ((object sqlite-persistent-object)
	    &optional (database *default-database*))
    (let ((command
	   (sqlite-persistent-class-delete-record-string (class-of object))))
      (assert command () "Delete record not possible for persistent class without primary key.")
      (destructuring-bind (statement-string in-slot-names) command
	  (exec (apply #'bind-parameters
		       (prepare database statement-string)
		       (slot-values object in-slot-names)))))
    object))

(defmethod table-name ((instance sqlite-persistent-object))
  (table-name (class-of instance)))

(defmethod schema-string ((instance sqlite-persistent-object))
  (schema-string (class-of instance)))

(defmethod primary-key ((instance sqlite-persistent-object))
  (let ((primary-key (primary-key (class-of instance))))
    (values
     primary-key
     (mapcar (lambda (slot) (slot-value instance slot)) primary-key))))

(defmethod foreign-keys ((instance sqlite-persistent-object))
  (let ((foreign-keys (foreign-keys (class-of instance))))
    (values
     foreign-keys
     (mapcar
      (lambda (foreign-key)
	(mapcar
	 (lambda (slot)
	   (slot-value instance slot))
	 (second foreign-key)))
      foreign-keys))))

;;; Select

(defgeneric pick (database count class sql &rest args)
  (:method (database count (class symbol) sql &rest args)
    (declare (dynamic-extent args))
    (apply #'pick database count (find-class class) sql args))
  (:method (database count (class sqlite-persistent-class) sql &rest args)
    (declare (dynamic-extent args))
    (ensure-finalized class)
    (destructuring-bind (select-string out-slot-names)
	(sqlite-persistent-class-select-string class)
      (declare (list out-slot-names))
      (with-open-query
	  (s (apply #'bind-parameters
		    (prepare database (concatenate 'string select-string sql))
		    args))
	(if count
	    (progn
	      (loop
		 repeat count
		 with row = (make-list (length out-slot-names))
		 for res = (read-row-into-sequence s row nil :eos)
		 until (eq res :eos)
		 collect (set-slot-values (make-instance class)
					  out-slot-names row)))
	    (loop
	       with row = (make-list (length out-slot-names))
	       for res = (read-row-into-sequence s row nil :eos)
	       until (eq res :eos)
	       collect (set-slot-values (make-instance class)
					out-slot-names row)))))))

(defun split-select-args (args)
  (do* ((sql-args nil (cons el sql-args))
	(rest args (cdr rest))
	(el (car args) (car rest)))
       ((or (null rest)
	    (eq el :count) (eq el :database))
	(values (nreverse sql-args)
		rest))))

(defun parse-select-args (args)
  (multiple-value-bind (sql-args keyword-args) (split-select-args args)
    (destructuring-bind (&key count (database nil database-p))
	keyword-args
      (values (first sql-args)
	      (rest sql-args)
	      count
	      database
	      database-p))))

(defun select (class &rest args)
  "A convenience function for SELECT."
  (multiple-value-bind (sql-string sql-args count database database-p)
      (parse-select-args args)
    (if database-p
	(apply #'pick database count class sql-string sql-args)
	(apply #'pick *default-database* count class sql-string sql-args))))

(define-compiler-macro select (class &rest args)
  (multiple-value-bind (sql-string sql-args count database database-p)
      (parse-select-args args)
    (if database-p
	`(pick ,database ,count ,class ,sql-string ,@sql-args)
	`(pick *default-database* ,count ,class ,sql-string ,@sql-args))))

(defgeneric slot-column-name (persistent-class slot-name)
  (:method ((persistent-class symbol) slot-name)
    (slot-column-name (find-class persistent-class) slot-name))
  (:method ((persistent-class sqlite-persistent-class) slot-name)
    (let ((slotd (find-slotd persistent-class slot-name)))
      (if slotd
	  (sqlite-persistent-slot-definition-column-name slotd)
	  (error "Slot ~A not found in class ~S."
		 slot-name
		 persistent-class)))))

(defgeneric create-table (persistent-class &optional database)
  (:method ((class sqlite-persistent-class)
	    &optional (database *default-database*))
    (exec (prepare database (schema-string class))))
  (:method ((class-name symbol) &optional (database *default-database*))
    (create-table (find-class class-name) database)))

;;; Reference

(declaim (inline reference-class-name
		 reference-local-slot-names
		 reference-referenced-slot-names))

(defun reference-class-name (foreign-key)
  (first foreign-key))

(defun reference-local-slot-names (foreign-key)
  (second foreign-key))

(defun reference-referenced-slot-names (foreign-key)
  (third foreign-key))

(defgeneric reference (reference-class instance &optional database)
  (:method ((reference-class-name symbol)
	    (instance sqlite-persistent-object)
	    &optional (database *default-database*))
    (let* ((class (class-of instance))
	   (foreign-key (assoc reference-class-name (foreign-keys class))))
      (if foreign-key
	  (let ((reference-instance
		 (make-instance (find-class reference-class-name))))
	    (mapc (lambda (slot-name reference-slot-name)
		    (setf (slot-value reference-instance reference-slot-name)
			  (slot-value instance slot-name)))
		  (reference-local-slot-names foreign-key)
		  (reference-referenced-slot-names foreign-key))
	    (update-from-record reference-instance database))
	  (error "No foreign key found for instance ~S~%and reference class ~A."
		 instance reference-class-name))))
  (:method ((reference-class sqlite-persistent-class)
	    (instance sqlite-persistent-object)
	    &optional (database *default-database*))
    (reference (class-name reference-class) instance database)))
