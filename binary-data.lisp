(cl:in-package :common-lisp-user)

(defpackage :ch.codehome.binary-data
  (:use :common-lisp :ch.codehome.practical)
  (:export :define-binary-class
		   :define-tagged-binary-class
		   :define-binary-type
		   :read-value
		   :write-value
		   :*in-progress-objects*
		   :parent-of-type
		   :current-binary-object
		   :+null+))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
	`(,name :initarg ,(as-keyword name) :accessor ,name)))

(defmacro define-binary-class (name slots)
  `(defclass ,name ()
	 ,(mapcar #'slot->defclass-slot slots)))


