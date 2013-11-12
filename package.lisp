;;;; -*- coding: utf-8-unix -*-
;;;; package.lisp.newest

(defpackage #:wm-sqlite
  (:use #:cl #:alexandria #:cffi #:trivial-garbage #:trivial-gray-streams
	#:closer-mop)
  (:shadowing-import-from #:closer-mop
			  #:standard-method
			  #:standard-generic-function
			  #:defmethod
			  #:defgeneric
			  #:standard-class)
  (:export
   ;; Error
   #:sqlite-error
   #:sqlite-error-code
   #:sqlite-error-extended-code
   
   ;; Database
   #:database
   #:open-database
   #:close-database
   #:with-open-database
   #:begin-transaction
   #:commit-transaction
   #:rollback-transaction
   #:with-transaction
   
   ;; Statement
   #:statement
   #:prepare
   #:bind-parameter
   #:bind-parameters
   #:clear-bindings
   #:exec
   #:last-insert-rowid

   ;; Blob Stream
   #:zeroblob
   #:blob-stream
   #:blob-output-stream
   #:blob-input-stream ; continue documentation here
   #:blob-io-stream
   #:open-blob
   #:blob-length
   #:blob-reopen
   #:with-open-blob

   ;; Query Stream
   #:query-stream
   #:query-input-stream
   #:open-query
   #:with-open-query
   #:read-sequence
   #:file-position
   #:read-row
   #:read-row-into-sequence
   #:read-row-column
   #:column-name
   #:column-names
   #:column-type
   #:column-types
   #:column-count

   ;; Convenience
   #:*default-database*
   #:with-database
   #:query

   ;; Templates
   #:to-sql
   #:sql-template
   #:ls
   #:cn
   #:tn

   ;; Persistent Objects
   #:sqlite-persistent-class
   #:create-table
   #:schema-string
   #:primary-key
   #:foreign-keys
   #:table-name
   #:last-used-key
   #:slot-column-name
   #:select
   #:pick
   #:update-record
   #:update-from-record
   #:insert-record
   #:delete-record
   #:reference

   ;; Statement Caching
   #:statement-caching-mixin))
