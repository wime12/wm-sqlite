;;;; -*- coding: utf-8-unix -*-
;;;; wm-sqlite-ffi.lisp.newest

(in-package #:wm-sqlite)

(define-foreign-library sqlite3
  (:windows "sqlite3.dll")
  (:darwin "/usr/local/Cellar/sqlite/3.8.0.2/lib/libsqlite3.dylib")
  (t (:default "libsqlite3")))

(use-foreign-library sqlite3)

;;; Errors

(defcfun sqlite3-extended-errcode
    :int
  (db :pointer))

(defcfun sqlite3-errstr
    (:string :encoding :utf-8)
  (code :int))

(defcfun sqlite3-extended-result-codes
    :int
  (handle :pointer)
  (on-off :boolean))

(defcfun sqlite3-errmsg
    (:string :encoding :utf-8)
  (db :pointer))

;; Open Flags

(defconstant +sqlite-open-readonly+ #x1)
(defconstant +sqlite-open-readwrite+ #x2)
(defconstant +sqlite-open-create+ #x4)

(defcfun (sqlite3-open-v2% "sqlite3_open_v2")
    :int
  (filename (:string :encoding :utf-8))
  (handle-ptr :pointer)
  (flags :int)
  (vfs-module :pointer))

(defun sqlite3-open-v2 (filename flags &optional (vfs-module (null-pointer)))
  (with-foreign-object (handle-ptr :pointer)
    (let ((error-code (sqlite3-open-v2% filename handle-ptr flags vfs-module)))
      (values (mem-ref handle-ptr :pointer)
	      error-code))))

(defcfun sqlite3-close-v2
    :int
  (handle :pointer))

;;; Statements

(defcfun (sqlite3-prepare% "sqlite3_prepare_v2")
    :int
  (db-handle :pointer)
  (sql-str (:string :encoding :utf-8))
  (max-length :int)
  (stmt-ptr :pointer)
  (tail-ptr :pointer))

(defun sqlite3-prepare (db-handle sql-str max-length)
  (with-foreign-objects ((stmt-ptr :pointer)
			 (tail-ptr :pointer))
    (let ((error-code
	   (sqlite3-prepare% db-handle sql-str max-length stmt-ptr tail-ptr)))
      (values (mem-ref stmt-ptr :pointer)
	      (mem-ref tail-ptr '(:string :encoding :utf-8))
	      error-code))))

(defcfun sqlite3-finalize
    :int
  (stmt :pointer))


;;; Exec

(defconstant +sqlite-row+ 100)
(defconstant +sqlite-done+ 101)

(defcfun sqlite3-step
    :int
  (stmt :pointer))

(defcfun sqlite3-column-count
    :int
  (stmt :pointer))

(defcfun sqlite3-column-type
    :int
  (stmt :pointer)
  (index :int))

(defconstant +sqlite-integer+ 1)
(defconstant +sqlite-float+ 2)
(defconstant +sqlite-text+ 3)
(defconstant +sqlite-blob+ 4)
(defconstant +sqlite-null+ 5)

(defcfun (sqlite3-column-int "sqlite3_column_int64")
    :int64
  (stmt :pointer)
  (index :int))

(defcfun sqlite3-column-double
    :double
  (stmt :pointer)
  (index :int))

(defcfun sqlite3-column-text
    (:string :encoding :utf-8)
  (stmt :pointer)
  (index :int))

(defcfun (sqlite3-column-blob% "sqlite3_column_blob")
    :pointer
  (stmt :pointer)
  (index :int))

(defcfun sqlite3-column-bytes
    :int
  (stmt :pointer)
  (index :int))

(defun sqlite3-column-blob (stmt index)
  (let* ((blob (sqlite3-column-blob% stmt index))
	 (len (sqlite3-column-bytes stmt index))
	 (res (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len res)
      (setf (aref res i) (mem-aref blob :unsigned-char i)))))

(defcfun sqlite3-column-name
    (:string :encoding :utf-8)
  (stmt :pointer)
  (index :int))

(defcfun sqlite3-reset
    :int
  (stmt :pointer))

(defcfun sqlite3-clear-bindings
    :int
  (stmt :pointer))

(defconstant +destructor-transient-address+
  (mod -1 (expt 2 (* 8 (foreign-type-size :pointer)))))

(defun destructor-transient ()
    (make-pointer +destructor-transient-address+))

(defcfun (sqlite3-bind-blob% "sqlite3_bind_blob")
    :int
  (stmt :pointer)
  (index :int)
  (data :pointer)
  (count :int)
  (destructor :pointer))

(defun sqlite3-bind-blob (stmt index value)
  (with-pointer-to-vector-data (blob value)
    (sqlite3-bind-blob% stmt index blob (length value) (destructor-transient))))

(defcfun (sqlite3-bind-text% "sqlite3_bind_text")
    :int
  (stmt :pointer)
  (index :int)
  (value (:string :encoding :utf-8))
  (count :int)
  (destructor :pointer))

(defun sqlite3-bind-text (stmt index value)
  (sqlite3-bind-text% stmt index value -1 (destructor-transient)))

(defcfun sqlite3-bind-int
    :int
  (stmt :pointer)
  (index :int)
  (value :int64))

(defcfun sqlite3-bind-double
    :int
  (stmt :pointer)
  (index :int)
  (value :double))

(defcfun sqlite3-bind-null
    :int
  (stmt :pointer)
  (index :int))

(defcfun sqlite3-bind-parameter-index
    :int
  (stmt :pointer)
  (name (:string :encoding :utf-8)))


;;; Blobs

(defcfun (sqlite3-blob-open% "sqlite3_blob_open")
    :int
  (db-handle :pointer)
  (database-name (:string :encoding :utf-8))
  (table-name (:string :encoding :utf-8))
  (column-name (:string :encoding :utf-8))
  (row :int64)
  (flags :int)
  (blob-handle-ptr :pointer))

(defun sqlite3-blob-open (db-handle database table column row flags)
  (with-foreign-object (blob-ptr :pointer)
    (let ((error-code
	   (sqlite3-blob-open% db-handle database table column
			       row flags blob-ptr)))
      (values (mem-ref blob-ptr :pointer)
	      error-code))))

(defcfun sqlite3-blob-close
    :int
  (blob-handle :pointer))

(defcfun sqlite3-blob-bytes
    :int
  (blob-handle :pointer))

(defcfun sqlite3-blob-read
    :int
  (blob-handle :pointer)
  (buffer :pointer)
  (n :int)
  (offset :int))

(defun sqlite3-blob-read-byte (blob-handle offset)
  (with-foreign-object (b :unsigned-char)
    (let ((error-code (sqlite3-blob-read blob-handle b 1 offset)))
      (values (mem-aref b :unsigned-char)
	      error-code))))

(defun sqlite3-blob-read-vector (blob-handle vec n offset)
  (with-pointer-to-vector-data (buf vec)
    (sqlite3-blob-read blob-handle buf n offset)))

(defcfun sqlite3-blob-reopen
    :int
  (blob-handle :pointer)
  (row :int64))

(defcfun sqlite3-blob-write
    :int
  (blob-handle :pointer)
  (buffer :pointer)
  (n :int)
  (offset :int))

(defun sqlite3-blob-write-byte (blob-handle byte offset)
  (with-foreign-object (b :unsigned-char)
    (setf (mem-aref b :unsigned-char) byte)
    (sqlite3-blob-write blob-handle b 1 offset) ))

(defun sqlite3-blob-write-vector (blob-handle vec n offset)
  (with-pointer-to-vector-data (buf vec)
    (sqlite3-blob-write blob-handle buf n offset)))

(defcfun sqlite3-bind-zeroblob
    :int
  (stmt :pointer)
  (index :int)
  (count :int))


;;; Utilities

(defcfun sqlite3-last-insert-rowid
    :int
  (db :pointer))

(defcfun sqlite3-changes
    :int
  (db :pointer))
