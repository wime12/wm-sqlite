(in-package #:wm-sqlite)

(defconstant +template-char+ #\`)

(defun split-string (character str)
  (let ((pos (position character str)))
    (if pos
      (values
       (subseq str 0 pos)
       (subseq str pos))
      (values str nil))))

(defun template-p (str)
  (char= (aref str 0) +template-char+))

(defun read-template (str)
  (multiple-value-bind (temp characters)
      (read-from-string (subseq str 1) t nil :preserve-whitespace t)
    (values
     temp
     (subseq str (1+ characters)))))

(defclass sql-list ()
  ((elements :reader sql-list-elements :initarg :elements :initform nil)))

(defclass sql-list-space (sql-list) ())

(defclass sql-list-comma (sql-list) ())

(defclass sql-list-space-parens (sql-list) ())

(defclass sql-list-comma-parens (sql-list) ())

(defgeneric to-sql (x)
  (:method ((x null))
    "null")
  (:method ((x symbol))
    (symbol-name x))
  (:method ((x string))
    (format nil "'~A'" x))
  (:method ((x integer))
    (princ-to-string x))
  (:method ((x float))
    (format nil "~,,,,,,'eE" x))
  (:method ((x sql-list-space))
    (format nil "~{~A~^ ~}"
	    (mapcar #'to-sql (sql-list-elements x))))
  (:method ((x sql-list-space-parens))
    (format nil "(~{~A~^ ~})"
	    (mapcar #'to-sql (sql-list-elements x))))
  (:method ((x sql-list-comma))
    (format nil "~{~A~^, ~}"
	    (mapcar #'to-sql (sql-list-elements x))))
  (:method ((x sql-list-comma-parens))
    (format nil "(~{~A~^, ~})"
	    (mapcar #'to-sql (sql-list-elements x)))))

(defun ls (elements &key comma parens)
  (make-instance
   (if comma
       (if parens
	   'sql-list-comma-parens
	   'sql-list-comma)
       (if parens
	   'sql-list-space-parens
	   'sql-list-space))
   :elements elements))

(defmacro sql-template (str)
  `(concatenate 'string	,@(expand-template str)))

(defun expand-template (str)
  (cond ((= (length str) 0) nil)
	((template-p str)
	 (multiple-value-bind (template rest) (read-template str)
	   (cons `(to-sql ,template) (expand-template rest))))
	(t (multiple-value-bind (s rest) (split-string +template-char+ str)
	     (cons s (expand-template rest))))))

(declaim (inline cn))

(defun cn (class-name slot-name)
  (make-symbol (slot-column-name class-name slot-name)))

(set-dispatch-macro-character #\# #\Q
   #'(lambda (stream subchar arg)
       (declare (ignore subchar arg))
       `(sql-template ,(read stream))))
