(defpackage eggqulibrium.parser
  (:use :cl)
  (:import-from :eggqulibrium.model
		:entry
		:entry-reference
		:entry-db :add-entry
  (:import-from :cl-csv
		:read-csv)
  (:export :parse-entries))
(in-package :eggqulibrium.parser)

(defun safe-parse-integer (data)
  (let ((res (parse-integer data :junk-allowed t)))
    (unless res
      (setf res 0))
    res))

(defun parse-boolean (data)
  (or (string= data "yes")
      (string= data "y")
      (string= data "t")
      (string= data "true")
      (string= data "0")))

(defun parse-entries (filename &key (skip-first t))
  (let ((db (make-instance 'entry-db)))
    (read-csv filename
	    :skip-first-p skip-first
	    :map-fn (lambda (row)
		      (add-entry db
				 (make-instance 'entry
						:recipe (nth 0 row)
						:yolks (safe-parse-integer (nth 1 row))
						:whites (safe-parse-integer (nth 2 row))
						:source (make-instance 'entry-reference
								       :name (nth 3 row)
								       :author (nth 4 row)
								       :url (nth 5 row)
								       :isbn (safe-parse-integer (nth 6 row))
								       :page (safe-parse-integer (nth 7 row)))
						:tried (parse-boolean (nth 8 row))))))
    db))
