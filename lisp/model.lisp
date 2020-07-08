(defpackage eggqulibrium.model
  (:use :cl)
  (:import-from :grip.message :export-message)
  (:export :configuration
	   ;; entry database/models
	   :entry :entry-recipe :entry-yolks :entry-whites :entry-source :entry-tried
	   :entry-reference :entry-ref-name :entry-ref-author :entry-ref-url :entry-ref-isbn :entry-ref-page
	   :entry-db :add-entry :db-length :db-primary :db-yolks :db-whites
	   ;; higher level interfaces
	   :find-equalibrium
	   :export-message))
(in-package :eggqulibrium.model)

(defclass configuration ()
  ((number-yolks
    :initarg :yolks
    :initform 0
    :reader conf-yolks)
   (number-whites
    :initarg :whites
    :initform 0
    :reader conf-whites))
  (:documentation "configuration model"))

(defclass entry ()
  ((recipe :reader entry-recipe :initarg :recipe :type string)
   (yolks :reader entry-yolks :initarg :yolks :type integer)
   (whites :reader entry-whites :initarg :whites :type integer)
   (source :reader entry-source :initarg :source :type entry-reference)
   (tried :reader entry-tried :initarg :tried :type boolean))
  (:documentation "model for a single partial egg consuming recpie"))

(defmethod export-message ((record entry))
  (let ((ht (make-hash-table :test 'eql)))
    (setf (gethash "recpie" ht) (entry-recipe record))
    (setf (gethash "source" ht) (entry-ref-name (entry-source record)))
    (setf (gethash "yolks" ht) (entry-yolks record))
    (setf (gethash "whites" ht) (entry-whites record))

    (grip:new-message ht :level grip.level:+info+)))

(defclass entry-reference ()
  ((name :reader entry-ref-name :initarg :name :type string)
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

(defgeneric add-entry (db entry)
  (:documentation "ingestion method for egg equilibrium entry"))

(defmethod add-entry ((db entry-db) (record entry))
  (setf (gethash (db-length db) (db-primary db)) record)

  (add-egg-part-to-table (db-whites db) (entry-whites record) record)
  (add-egg-part-to-table (db-yolks db) (entry-yolks record) record)

  (incf (db-length db)))


(defun add-egg-part-to-table (ht num entry)
  (when (= 0 num)
    (return-from add-egg-part-to-table))

  (when (gethash num ht nil)
    (vector-push-extend entry (gethash num ht))
    (return-from add-egg-part-to-table))

  (let ((vec (make-array 1 :fill-pointer 0 :adjustable t)))
    (vector-push-extend entry vec)
    (setf (gethash num ht) vec)))

(defgeneric find-equalibrium (configuration database)
  (:documentation "Implementations of find-implementation take a
  configuration and return sequence of entries (i.e. recpies) that can
  complete the solution."))

(defun needs-yolks ()
  (grip:info> "needs yolks"))
(defun needs-whites ()
  (grip:info> "needs whites"))


(defmethod get-parts ((db entry-db) (part symbol) (num integer))
  (case part
    (:yolks
     (let ((entries (make-array 0 :adjustable t :fill-pointer 0))
	   (yolks (db-yolks db)))
       (loop for count from num downto 1 do
	 (when (= 0 num)
	   (return-from get-parts entries))

	 (multiple-value-bind (slice ok) (gethash count yolks)
	   (when ok
	     (vector-push-extend (aref slice (random (length slice))) entries))
	   (decf num count)))))
     (grip:info> "yolk")
    (:whites
     (grip:info> "white"))
    (otherwise
     (grip:error> (grip:new-message "oh no ~A" :args (list part))))))

(defun switch-test (part)
  (case part
    (:yolk
     (grip:info> "yolk"))
    (:whites
     (grip:info> "white"))
    (otherwise
     (grip:error> "oh no"))))

(defun smallest-positive-delta (a b)
  (if (> a b)
      (- a b)
      (- b a)))


(defmethod find-equalibrium ((conf configuration) (db entry-db))
  (with-accessors ((yolks conf-yolks) (whites conf-whites)) conf
    (when (= yolks whites)
      (grip:info> "eggqulibrium exists!")
      (return-from find-equalibrium t))

    (let ((part-type (if (> yolks whites) :yolks :whites))
	  (output (make-array 0 :adjustable t :fill-pointer 0)))
      (loop for item across (get-parts db part-type (smallest-positive-delta yolks whites))
	    do
	       (vector-push-extend item output)
	       (grip:info> item))
      output)))
