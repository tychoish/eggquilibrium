(defpackage eggqulibrium.model
  (:use :cl)
  (:export :configuration
	   :entry :entry-recipe :entry-yolks :entry-whites :entry-source :entry-tried
	   :entry-reference :entry-ref-name :entry-ref-author :entry-ref-url :entry-ref-isbn :entry-ref-page
	   :entry-db :add-entry :db-length :db-primary :db-yolks :db-whites))
(in-package :eggqulibrium.model)

(defclass configuration ()
  ()
  (:documentation "configuration model"))

(defclass entry ()
  ((recipe :reader entry-recipe :initarg :recipe :type string)
   (yolks :reader entry-yolks :initarg :yolks :type integer)
   (whites :reader entry-whites :initarg :whites :type integer)
   (source :reader entry-source :initarg :source :type entry-reference)
   (tried :reader entry-tried :initarg :tried :type boolean))
  (:documentation "model for a single partial egg consuming recpie"))

(defclass entry-reference ()
  ((name :reader entry-ref-name :initarg :name :type string )
   (author :reader entry-ref-author :initarg :author :type string)
   (url :reader entry-ref-url :initarg :url :type string)
   (isbn :reader entry-ref-isbn :initarg :isbn :type string)
   (page :reader entry-ref-page :initarg :page :type string))
  (:documentation "reference object for entries"))

(defclass entry-db ()
  ((primary :accessor db-primary :initform (make-hash-table) :type hash-table)
   (yolks :accessor db-yolks :initform (make-hash-table) :type hash-table)
   (whites :accessor db-whites :initform (make-hash-table) :type hash-table)
   (length :initform 0 :accessor db-length :type integer)))

(defgeneric add-entry (db entry))

(defmethod add-entry ((db entry-db) (record entry))
  (setf (gethash (db-length db) (db-primary db)) record)

  (add-egg-part-for-id (db-whites db) (db-length db) (entry-whites record))
  (add-egg-part-for-id (db-yolks db) (db-length db) (entry-yolks record))

  (incf (db-length db)))

(defun add-egg-part-for-id (ht id num)
  (when (= 0 num)
    (return-from add-egg-part-for-id))

  (when (gethash num ht nil)
    (vector-push-extend id (gethash num ht))
    (return-from add-egg-part-for-id))

  (let ((vec (make-array 1 :fill-pointer 0 :adjustable t)))
    (vector-push-extend id vec)
    (setf (gethash num ht) vec)))
