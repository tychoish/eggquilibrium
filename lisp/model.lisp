(defpackage eggqulibrium.model
  (:use :cl)
  (:import-from :grip.message :export-message)
  (:export :configuration
	   ;; entry database/models
	   :entry :entry-recipe :entry-yolks :entry-whites :entry-source :entry-tried
	   :entry-reference :entry-ref-name :entry-ref-author :entry-ref-url :entry-ref-isbn :entry-ref-page
	   :entry-db :add-entry :db-length :db-primary :db-yolks :db-whites
	   ;; higher level interfaces
	   :find-equilibrium
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
  (:documentation "base configuration model"))

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

(defun shuffle-vector (vector)
  (loop for idx downfrom (1- (length vector)) to 1
	for other = (random (1+ idx))
	do (unless (= idx other)
	     (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun add-egg-part-to-table (ht num entry)
  (when (= 0 num)
    (return-from add-egg-part-to-table))

  (when (gethash num ht nil)
    (vector-push-extend entry (gethash num ht))
    (return-from add-egg-part-to-table))

  (let ((vec (make-array 1 :fill-pointer 0 :adjustable t)))
    (vector-push-extend entry vec)
    (setf (gethash num ht) vec)))

(defgeneric find-equilibrium (configuration database)
  (:documentation "Implementations of find-implementation take a
  configuration and return sequence of entries (i.e. recpies) that can
  complete the solution."))

(defgeneric equilibriump (conf))

(defmethod equilibriump ((conf configuration))
  (= (conf-yolks conf) (conf-whites conf)))

(defmethod replan-equalib ((conf configuration) results)
  (let ((yolks (conf-yolks conf))
	(whites (conf-whites conf)))

    (loop for item across results
	  do
	     (incf whites (entry-whites item))
	     (incf yolks (entry-yolks item)))

    (make-instance 'configuration :whites whites :yolks yolks)))

(defun entries-for-parts (records num results)
  (loop for count from num downto 1 do
    (multiple-value-bind (slice ok) (gethash count records)
      (when ok
	(vector-push-extend (aref slice (random (length slice))) results)
	(decf num count)))))

(defmethod find-equilibrium ((conf configuration) (db entry-db))
  (let* ((output (make-array 0 :adjustable t :fill-pointer 0))
	 (state (replan-equalib conf output))
	 (iters -1))
    (loop
      (incf iters)
      (let ((state (replan-equalib state output)))
	(when (equilibriump state)
	  (grip:info> "eggqulibrium eggsists!")
	  (grip:notice> (list (cons "eggs" (conf-yolks state))
			      (cons "start-yolk" (conf-yolks conf))
			      (cons "start-whites" (conf-whites conf))
			      (cons "yolk" (- (conf-yolks state) (conf-yolks conf)))
			      (cons "iterations" iters)
			      (cons "whites" (- (conf-whites state) (conf-whites conf)))))
	  (return-from find-equilibrium output))

	(when (>= (length output) (hash-table-size (db-primary db)))
	  (grip:warning> "could not find eggqulibrium")
	  (return-from find-equilibrium output))

	(let ((yolks (conf-yolks state))
	      (whites (conf-whites state))
	      (previous (length output)))

	  (grip:debug> (grip:new-message
			(list (cons "yolks" yolks)
			      (cons "whites" whites)
			      (cons "records" (length output)))
			:when (> previous 0)))

	  (if (> yolks whites)
	      (entries-for-parts (db-whites db) (- yolks whites) output)
	      (entries-for-parts (db-yolks db) (- whites yolks) output))

	  (when (= previous (length output))
	    (grip:warning> (list (cons "message" "unsolveable eggqulibrium problem, not making progress!")
				 (cons "found" (length output))))

	    (return-from find-equilibrium output))

	  (setf state (replan-equalib conf output)))))))
