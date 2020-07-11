(defpackage eggqulibrium.operations
  (:use :cl)
  (:import-from :eggqulibrium.parser
		:parse-entries)
  (:import-from :eggqulibrium.model
		:configuration-additive
		:configuration-utilization
		:find-equilibrium
		:db-primary)
  (:export :main))
(in-package :eggqulibrium.operations)

(defun log-fatal (msg)
  (grip:emergency> msg)
  (uiop:quit 112))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun handle-missing-option (condition)
  (grip:error> (format nil "~A" condition))
  (log-fatal "missing required argument"))

(defun handle-option-parse-failure (condition)
  (grip:error> (format nil "~A" condition))
  (log-fatal "option parsing failure"))

(defun handle-missing-required-option (condition)
  (grip:error> (format nil "~A" condition))
  (log-fatal "missing required "))

(defun parse-mode (value)
  (when (search "util" value)
    (return-from parse-mode "utilization"))

  (when (search "add" value)
    (return-from parse-mode "additive"))

  (log-fatal (grip:new-message "~A is not a known mode" :args value)))

(opts:define-opts
  (:name :help
   :description "print help text"
   :short #\h
   :long "help")
  (:name :path
   :description "path to input csv file"
   :long "path"
   :short #\p
   :default (merge-pathnames (uiop:getcwd) "eggqulibrium.csv"))
  (:name :mode
   :description "search for equilibrium in 'additive' mode (add
   recpies to get a new equilibrium) or utilization (find recpies to
   use specified egg parts)."
   :long "mode"
   :short #\m
   :default "additive"
   :arg-parser #'parse-mode)
  (:name :yolks
   :description "number of yolks"
   :arg-parser #'parse-integer
   :long "yolks"
   :default 0)
  (:name :whites
   :description "number of whites"
   :arg-parser #'parse-integer
   :long "whites"
   :default 0))


(defun configuration-factory (mode yolks whites)
  (when (equal "additive" mode)
    (return-from configuration-factory
      (make-instance 'configuration-additive :yolks yolks :whites whites)))
  (when (equal "utilization" mode)
    (return-from configuration-factory
      (make-instance 'configuration-utilization :yolks yolks :whites whites)))

  (log-fatal (grip:new-message "~A is not a known mode" :args mode)))

(defun main (&rest args)
  (declare (ignorable args))
  (setf grip:*default-logger* (make-instance 'grip.logger:stream-journal :name (pathname-name (uiop:argv0))))

  (multiple-value-bind (options free-args)
      (handler-bind ((opts:missing-arg #'handle-missing-option)
		     (opts:arg-parser-failed #'handle-option-parse-failure)
		     (opts:missing-required-option #'handle-missing-required-option))
	(opts:get-opts))
    (when-option (options :help)
      (opts:describe
       :prefix "eggqulibirium is a partial egg utilization recpie finder."
       :usage-of (uiop:argv0)
       :args     free-args)
      (return-from main))

    (grip:debug> (list :filename (getf options :path)))

    (let* ((db (parse-entries (getf options :path)))
	   (conf (configuration-factory (getf options :mode) (getf options :yolks) (getf options :whites)))
	   (results (find-equilibrium conf db)))

      (loop for item across results
	    do
	       (grip:info> item)))))
