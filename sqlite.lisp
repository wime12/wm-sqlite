;;;; -*- coding: utf-8-unix -*-
;;;; wm-sqlite-ffi.lisp.newest

(in-package #:wm-sqlite)

(declaim (optimize (speed 3) (space 0)))


;;; Wrapper

(defgeneric handle (wrapper))

(defclass wrapper ()
  ((handle :reader handle :initarg :handle
	   :initform (error "No handle provided."))))

(declaim (inline get-handle-and-check))

(defun get-handle-and-check (stream)
  (or (slot-value stream 'handle) (error "~s is closed" stream)))


;;; Database

(defvar *default-database*)

(defclass database (wrapper)
  ((filename :reader database-filename :initarg :filename))
  (:documentation "A class that represents connections to sqlite3 databases."))

(defmethod print-object ((instance database) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (unless (handle instance)
      (prin1 :closed stream)
      (princ " " stream))
    (prin1 (database-filename instance) stream)))


;; SQLite Errors

(define-condition sqlite-error (error)
  ((code
    :reader sqlite-error-code
    :initarg :code)
   (extended-code
    :reader sqlite-error-extended-code
    :initarg :extended-code
    :initform nil)
   (database
    :reader sqlite-error-database
    :initarg :database
    :initform nil))
  (:report
   (lambda (c stream)
     (princ (sqlite3-errstr (sqlite-error-code c)) stream)
     (let ((database (sqlite-error-database c)))
       (when (and database (/= (sqlite-error-code c)
			       (sqlite-error-extended-code c))) 
	 (princ ": " stream)
	 (princ (sqlite3-errmsg (handle database)) stream)))))
  (:documentation "A condition for errors signalled by the sqlite3 library."))

(defconstant +sqlite-constraint+ 19)

(define-condition sqlite-constraint-error (sqlite-error)
  ())

(define-condition database-closed-error (error)
  ((database :initarg :database :reader database-closed-error-database))
  (:report (lambda (c stream)
	     (format stream "Database is closed: ~S."
		     (database-closed-error-database c)))))

(defun check-sqlite-error (code &optional database)
  (declare (fixnum code))
  (unless (zerop code)
    (error (make-condition
	    (if (= code +sqlite-constraint+)
		'sqlite-constraint-error
		'sqlite-error)
	    :code code
	    :extended-code (when database
			     (sqlite3-extended-errcode (handle database)))
	    :database database))))

;; Open Flags

(defconstant +sqlite-readonly+ #x1)
(defconstant +sqlite-open-readwrite+ #x2)
(defconstant +sqlite-open-create+ #x4)
(defconstant +sqlite-open-readwrite-create+
  (logior +sqlite-open-readwrite+ +sqlite-open-create+))

(defun open-database (filename
		      &key (mode :read-write-create) (class 'database))
  "Opens an SQL database and returns a database object. The first argument
is any valid lisp pathname or one of SQLite's special filenames like e.g.
\":memory:\". The following keywords may be passed as second argument:
:READ (read access only), :READ-WRITE (read and write access) or
:READ-WRITE-CREATE (like before but also creates the database if
necessary. :READ-WRITE-CREATE is the default."
  (let ((filename (etypecase filename
		    (pathname (namestring filename))
		    (string (if (char= (aref filename 0) #\:)
				filename
				(namestring
				 (translate-logical-pathname filename)))))))
    (multiple-value-bind (handle errcode)
	(sqlite3-open-v2
	 filename
	 (ecase mode
	   (:read +sqlite-open-readonly+)
	   (:read-write +sqlite-open-readwrite+)
	   (:read-write-create +sqlite-open-readwrite-create+)))
      (let ((database (make-instance class
				     :handle handle
				     :filename filename)))
	(check-sqlite-error errcode)
	database))))

(defgeneric close-database (database)
  (:method ((database database))
    (check-sqlite-error
     (sqlite3-close-v2 (handle database)))
    (with-slots (handle filename) database
      (setf handle nil)))
  (:documentation "Closes a database."))

(defmacro with-open-database ((db filename
				  &key (mode :read-write-create)
				  (class ''database))
			      &body body)
  "Opens a database and ensures that it will be closed after BODY
has been executed or if an error occurs in BODY. The first argument
is the name of the database by which the database can be referred to
in BODY. The other arguments are like those of OPEN-DATABASE."
  `(let ((,(if (eq db t) '*default-database* db)
	  (open-database ,filename :mode ,mode :class ,class)))
     (unwind-protect
	  (progn ,@body)
       (close-database ,db))))

;;; Statements

(defclass statement (wrapper)
  ((database
    :accessor statement-database
    :initarg :database))
  (:documentation "Instances of this class represent statements which have
been created by the generic function PREPARE. They can be executed
by the generic function EXEC or used to create QUERY-STREAMS."))

(defmethod initialize-instance :after ((instance statement)
				&rest initargs
				&key &allow-other-keys)
  (declare (ignore initargs))
  (finalize instance (let ((handle (handle instance)))
		       (lambda ()
			 (sqlite3-finalize handle)))))

;; Statement preparation

(defgeneric prepare (database sql &optional length)
  (:documentation
   "Prepares a statement in the database and returns an instance of
STATEMENT. The first argument is the DATABASE, the second argument
is the SQL string. Optionally a third argument may designate the
index up to which the SQL string will be considered. Only the first
full SQL statement in the SQL string is prepared. The rest of the
string is returned for further processing.")
  (:method ((database database) sql &optional (length -1))
    (multiple-value-bind (stmt-handle tail errcode)
	(sqlite3-prepare (get-handle-and-check database) sql length)
      (check-sqlite-error errcode database)
      (values
       (make-instance 'statement :handle stmt-handle :database database)
       tail))))

;; Parameter binding

(defgeneric bind-parameter (statement loc value)
  (:documentation
   "Binds the VALUE to a parameter of STATEMENT designated by LOC.
LOC can be an integer or a string. It either refers to the
index of the parameter in the SQL string or to the identifier
of the parameter.
Parameter bindings persist after the execution of the STATEMENT
and may be resused in following invocations.")
  (:method ((statement statement) (loc string) value)
    (bind-parameter statement
		    (sqlite3-bind-parameter-index (handle statement) loc)
		    value))
  (:method ((statement statement) (loc integer) (value integer))
    (check-sqlite-error
     (sqlite3-bind-int (handle statement) loc value))
    statement)
  (:method ((statement statement) (loc integer) (value float))
    (check-sqlite-error
     (sqlite3-bind-double (handle statement) loc value))
    statement)
  (:method ((statement statement) (loc integer) (value string))
    (check-sqlite-error
     (sqlite3-bind-text (handle statement) loc value))
    statement)
  (:method ((statement statement) (loc integer) (value vector))
    (check-sqlite-error
     (sqlite3-bind-blob (handle statement) loc
			(coerce value '(vector (unsigned-byte 8)))))
    statement)
  (:method ((statement statement) (loc integer) (value null))
    (declare (ignore value))
    (check-sqlite-error
     (sqlite3-bind-null (handle statement) loc))
    statement))

(defun bind-parameters (statement &rest args)
  "Binds a set of parameters to statement. The parameters are given
as pairs of parameter index or parameter identifier and the value,
e.g. '(1 1.234) or '(\"arg2\" 1.234). See BIND-PARAMETER for
further details."
  (declare (dynamic-extent args))
  (let ((index 0))
    (dolist (arg args statement)
      (if (and arg (listp arg))
	  (bind-parameter statement (first arg) (second arg))
	  (bind-parameter statement (incf index) arg)))))

(defgeneric clear-bindings (statement)
  (:method ((statement statement))
    (check-sqlite-error
     (sqlite3-clear-bindings (handle statement))
     (statement-database statement))
    statement)
  (:documentation "Clears all parameter bindings established by BIND-PARAMETER in STATEMENT."))

;; Exec

(defgeneric exec (statement)
  (:documentation "Executes a statement.")
  (:method ((statement statement))
    (let ((handle (handle statement)))
      (unwind-protect
	   (let ((errcode (sqlite3-step handle)))
	     (unless (or (= errcode +sqlite-row+) (= errcode +sqlite-done+))
	       (check-sqlite-error errcode (statement-database statement)))
	     nil)
	(sqlite3-reset handle)))))

(defun get-column (stmt index)
  (ecase (sqlite3-column-type stmt index)
    (#.+sqlite-integer+ (sqlite3-column-int stmt index))
    (#.+sqlite-float+ (sqlite3-column-double stmt index))
    (#.+sqlite-text+ (sqlite3-column-text stmt index)) 
    (#.+sqlite-blob+ (sqlite3-column-blob stmt index))
    (#.+sqlite-null+ nil)))

;;; Transactions

(defun begin-transaction (&optional (database *default-database*))
  "Starts a transaction."
  (exec (prepare database "begin transaction")))

(defun commit-transaction (&optional (database *default-database*))
  "Commits any changes made since the transaction has begun and
ends the transaction."
  (exec (prepare database "commit transaction")))

(defun rollback-transaction (&optional (database *default-database*))
  "Forgets all changes made since the transaction has begun and
ends the transaction."
  (exec (prepare database "rollback transaction")))

(defmacro with-transaction ((&optional (database '*default-database*))
			    &body body)
  "Starts a transaction and commits the transaction after the
execution of BODY has finished. Provides the restarts
COMMIT-TRANSACTION and ROLLBACK-TRANSACTION if an
error occurs in BODY."
  `(let ((,database ,(if (eq database t) *default-database* database)))
     (begin-transaction ,database)
     (restart-case
	 (progn
	   ,@body
	   (commit-transaction ,database))
       (commit-transaction ()
	 :report "Commit transaction keeping all changes."
	 (commit-transaction ,database))
       (rollback-transaction ()
	 :report "Rollback transaction abandoning all changes."
	 (rollback-transaction ,database)))))

;;; Blobs

;; Zeroblobs

(defclass zeroblob ()
  ((count :reader zeroblob-count :initarg :count
	  :initform (error
		     "The number of bytes in a zeroblob must be specified.")))
  (:documentation "Instances of this class represent zeroblobs which can be
written to records and later be filled by using BLOB-STREAMs."))

(defun zeroblob (count)
  "Creates a zeroblob with COUNT bytes."
  (make-instance 'zeroblob :count count))

(defmethod bind-parameter ((statement statement) (loc integer) (value zeroblob))
  (check-sqlite-error
   (sqlite3-bind-zeroblob (handle statement) loc (zeroblob-count value))))

;; Blob Streams

(defclass blob-stream (fundamental-binary-stream wrapper)
  ((position :reader stream-file-position :initarg :position :initform 0)
   (length :reader blob-length :initarg :length)
   (database :reader blob-stream-database :initarg :database))
  (:documentation "Base class for BLOP-OUTPUT-STREAMs and BLOP-INPUT-STREAMs.
Its element type is always (UNSIGNED-BYTE 8)."))

(defmethod initialize-instance :after ((instance blob-stream)
				       &rest initargs &key)
  (declare (ignore initargs))
  (with-slots (length handle) instance
    (setf length (sqlite3-blob-bytes handle))))

(defmethod print-object ((instance blob-stream) stream)
  (if (null (handle instance))
      (print-unreadable-object (instance stream :type t :identity t)
	(prin1 :closed stream))
      (call-next-method)))

(defmethod (setf stream-file-position) (pos (stream blob-stream))
  (with-slots (position length) stream
    (if (< pos length)
	(setf position pos)
	(error "Position too large for this blob-stream."))))

(defmethod stream-element-type ((stream blob-stream))
  '(unsigned-byte 8))

(defgeneric blob-reopen (stream row)
  (:documentation
   "Positions a BLOB-STREAM on another row in the current table.")
  (:method ((stream blob-stream) row)
    (let ((handle (get-handle-and-check stream)))
      (check-sqlite-error
       (sqlite3-blob-reopen handle row)
       (blob-stream-database stream))
      (with-slots (position length) stream
	(setf position 0)
	(setf length (sqlite3-blob-bytes handle))))))

(defclass blob-input-stream (blob-stream fundamental-binary-input-stream)
  ()
  (:documentation ""))

(defmethod stream-read-byte ((stream blob-input-stream))
  (let ((handle (get-handle-and-check stream)))
    (with-slots (position length) stream
      (if (< position length)
	  (multiple-value-bind (byte errcode)
	      (sqlite3-blob-read-byte handle position)
	    (check-sqlite-error errcode)
	    (incf position)
	    byte)
	  :eof))))

(defmethod stream-read-sequence ((stream blob-input-stream)
				 (sequence vector) start end &key)
  (cond ((and (typep sequence '(simple-array (unsigned-byte 8)))
	      (zerop start))
	 (let* ((bpos (file-position stream))
		(len (min end
			  (- (blob-length stream) bpos))))
	   (check-sqlite-error
	    (sqlite3-blob-read-vector (get-handle-and-check stream)
				      sequence len bpos)
	    (blob-stream-database stream))
	   (with-slots (position) stream
	     (incf position len))
	   len))
	(t (call-next-method))))

(defclass blob-output-stream (blob-stream fundamental-binary-output-stream)
  ())

(defmethod stream-write-byte ((stream blob-output-stream) byte)
  (let ((handle (get-handle-and-check stream)))
    (with-slots (position length) stream
      (check-sqlite-error
       (sqlite3-blob-write-byte handle byte position)
       (blob-stream-database stream))
      (incf position))))

(defmethod stream-write-sequence ((stream blob-output-stream)
				  (sequence vector) start end &key)
  (cond ((and (typep sequence '(simple-array (unsigned-byte 8)))
	      (zerop start))
	 (let* ((bpos (file-position stream)))
	   (check-sqlite-error
	    (sqlite3-blob-write-vector (get-handle-and-check stream)
				       sequence end bpos)
	    (blob-stream-database stream)) 
	   (with-slots (position) stream
	     (incf position end))
	   sequence))
	(t (call-next-method))))

(defclass blob-io-stream (blob-input-stream blob-output-stream)
  ())

(defgeneric open-blob (database table column row &key database-name direction)
  (:method ((database (eql t)) table column row
	    &key (database-name "main") (direction :input))
    (open-blob *default-database* table column row
	       :database-name database-name :direction direction))
  (:method ((database database) (table string) (column string) row
	    &key (database-name "main") (direction :input))
    (if (eq direction :probe)
      (handler-case
	  (let ((b (make-blob-stream database table column
				     row database-name direction)))
	    (close b)
	    b)
	(sqlite-error () nil))
      (make-blob-stream database table column row database-name direction))))

(defun make-blob-stream (database table column row database-name direction)
  (multiple-value-bind (handle errcode)
      (sqlite3-blob-open (get-handle-and-check database) database-name
			 table column row
			 (ecase direction
			   ((:input :probe) 0)
			   ((:output :io) 1)))
    (check-sqlite-error errcode database)
    (make-instance
     (ecase direction
       ((:input :probe) 'blob-input-stream)
       (:output 'blob-output-stream)
       (:io 'blob-io-stream))
     :handle handle
     :database database)))

(defmethod close ((stream blob-stream) &key abort)
  (declare (ignore abort))
  (with-slots (handle) stream
    (when handle
      (check-sqlite-error
       (sqlite3-blob-close handle))
      (setf handle nil)
      t)))

(defmacro with-open-blob ((stream database table column row
				  &key (database-name "main")
				  (direction :input))
			  &body body)
  `(let ((,stream (open-blob ,database ,table ,column ,row
			     :database-name ,database-name
			     :direction ,direction)))
     (unwind-protect
	  (progn ,@body)
       (close ,stream))))

;;; Query Streams

(defclass query-stream (fundamental-stream)
  ((statement :initarg :statement :reader query-stream-statement)
   (handle)))

(defmethod print-object ((instance query-stream) stream)
  (if (slot-value instance 'handle)
      (call-next-method)
      (print-unreadable-object (instance stream :type t :identity t)
	(prin1 :closed stream))))

(defmethod initialize-instance :after ((instance query-stream) &rest initargs &key)
  (declare (ignore initargs))
  (setf (slot-value instance 'handle) (handle (query-stream-statement instance))))

(defclass query-input-stream (query-stream fundamental-input-stream)
  ((column-count :initarg :column-count :reader column-count)
   (eof :initarg :eof :initform nil :reader query-input-stream-eof-p)))

(defmethod initialize-instance :after ((instance query-input-stream)
				       &rest initargs &key)
  (declare (ignore initargs))
  (setf (slot-value instance 'column-count)
	(sqlite3-column-count (slot-value instance 'handle))))

(defclass query-input-list-stream (query-input-stream)
  ())

(defclass query-input-vector-stream (query-input-stream)
  ())

(defgeneric open-query (statement &key &allow-other-keys)
  (:method ((statement statement) &key (element-type 'list))
    (make-instance (ecase element-type
		     (list 'query-input-list-stream)
		     (vector 'query-input-vector-stream))
		   :statement statement)))

(defmethod close ((stream query-stream) &key abort)
  (declare (ignore abort))
  (with-slots (statement handle) stream
    (when statement
      (sqlite3-reset handle)
      (setf statement nil
	    handle nil)
      t)))

(defmacro with-open-query ((stream statement &rest options)
			   &body body)
  `(let ((,stream (open-query ,statement ,@options)))
     (unwind-protect
	  (progn ,@body)
       (close ,stream))))

(defgeneric column-name (stream index)
  (:method ((stream query-input-stream) index)
    (sqlite3-column-name (get-handle-and-check stream) index)))

(defgeneric column-type (stream index)
  (:method ((stream query-input-stream) index)
    (number-to-type
     (sqlite3-column-type (get-handle-and-check stream) index))))

(defun number-to-type (n)
  (ecase n
    (#.+sqlite-integer+ :integer)
    (#.+sqlite-float+ :float)
    (#.+sqlite-text+ :text)
    (#.+sqlite-blob+ :blob)
    (#.+sqlite-null+ :null)))

(defgeneric read-row (stream &optional eof-error-p eof-value)
  (:method ((stream query-input-list-stream)
	    &optional (eof-error-p t) eof-value)
    (with-slots (column-count) stream
      (let ((handle (get-handle-and-check stream)))
	(generic-read-row stream handle eof-error-p eof-value
			  (lambda ()
			    (loop
			       for i from 0 below column-count
			       collect (get-column handle i)))))))
  (:method ((stream query-input-vector-stream)
	    &optional (eof-error-p t) eof-value)
    (with-slots (column-count) stream
      (let ((handle (get-handle-and-check stream)))
	(generic-read-row stream handle eof-error-p eof-value
			  (lambda ()
			    (let ((v (make-array column-count)))
			      (dotimes (i column-count v)
				(setf (aref v i) (get-column handle i))))))))))

(defgeneric read-row-column (stream column &optional eof-error-p eof-value)
  (:method ((stream query-input-stream) column
	    &optional (eof-error-p t) eof-value)
    (let ((handle (get-handle-and-check stream)))
      (generic-read-row stream handle eof-error-p eof-value
			(lambda ()
			  (get-column handle column))))))

(defgeneric read-row-into-sequence (sequence stream
				    &optional eof-error-p eof-value)
  (:method ((sequence list) (stream query-input-stream)
	    &optional (eof-error-p t) eof-value)
    (with-slots (column-count) stream
      (let ((handle (get-handle-and-check stream))
	    (count (min (column-count stream) (length sequence))))
	(generic-read-row stream handle eof-error-p eof-value
			  (lambda ()
			    (do ((tail sequence (cdr tail))
				 (i 0 (1+ i)))
				((>= i count) i)
			      (rplaca tail (get-column handle i))))))))
  (:method ((sequence vector) (stream query-input-stream)
	    &optional (eof-error-p t) eof-value)
    (with-slots (column-count) stream
      (let ((handle (get-handle-and-check stream))
	    (count (min (column-count stream) (length sequence))))
	(generic-read-row stream handle eof-error-p eof-value
			  (lambda ()
			    (dotimes (i count i)
			      (setf (aref sequence i)
				    (get-column handle i)))))))))

(defun generic-read-row (stream handle eof-error-p eof-value fn)
  (if (not (query-input-stream-eof-p stream))
      (let ((errcode (sqlite3-step handle)))
	(case errcode
	  (#.+sqlite-row+ (funcall fn))
	  (#.+sqlite-done+ (setf (slot-value stream 'eof) t)
			   (report-eof stream eof-error-p eof-value))
	  (otherwise
	   (check-sqlite-error errcode
			       (statement-database
				(query-stream-statement stream))))))
      (report-eof stream eof-error-p eof-value)))

(defun report-eof (stream eof-error-p eof-value)
  (if eof-error-p
      (error 'end-of-file :stream stream)
      eof-value))

(defmethod stream-read-sequence ((stream query-input-stream) (sequence list)
				 start end &key)
  (let ((count (- end start)))
    (do ((tail (nthcdr start sequence) (cdr tail))
	 (i 0 (1+ i)))
	((= i count) count)
      (let ((row (read-row stream nil :eof)))
	(if (eq row :eof)
	    (return i)
	    (rplaca tail row))))))

(defmethod stream-read-sequence ((stream query-input-stream)
				 (sequence vector)
				 start end &key)
  (loop
     for i from start below end
     for row = (read-row stream nil :eof)
     until (eq row :eof)
     do (setf (aref sequence i) row)
     finally (return i)))

;;; Convenience

(defmethod prepare ((database (eql t)) sql &optional (length -1))
  (prepare *default-database* sql length))

(defmethod close-database ((database (eql t)))
  (close-database *default-database*))

(defgeneric query (database sql &rest args)
  (:method ((database (eql t)) sql &rest args)
    (declare (dynamic-extent args))
    (apply #'query *default-database* sql args))
  (:method ((database database) sql &rest args)
    (declare (dynamic-extent args))
    (with-open-query (in
		      (apply #'bind-parameters (prepare database sql) args))
      (values
       (loop
	  for row = (read-row in nil :eof)
	  until (eq row :eof)
	  collect row)
       (loop
	  for i from 0 below (column-count in)
	  collect (column-name in i))))))


;;; Utilities

(defgeneric last-insert-rowid (database)
  (:method ((database (eql t)))
    (last-insert-rowid *default-database*))
  (:method ((database database))
    (sqlite3-last-insert-rowid (handle database))))

(defgeneric changes (database)
  (:method ((databse (eql t)))
    (changes *default-database*))
  (:method ((database database))
    (sqlite3-changes (handle database))))
