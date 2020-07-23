(defsystem "eggquilibrium"
  :description "Eggquilibrium helps you use eggs better"
  :author "Sam Kleinman <sam@tychoish.com>"
  :license  "Apache v2"
  :version "0.0.1"
  :depends-on ("cl-csv" "unix-opts" "cl-grip" "make-hash")
  :build-operation "program-op" ;; leave as is
  :build-pathname "build/eggquilibrium"
  :entry-point "eggquilibrium.operations:main"
  :components ((:module "lisp"
		:components
		((:file "model")
		 (:file "parser" :depends-on ("model"))
		 (:file "operations" :depends-on ("model" "parser")))))
  :in-order-to ((test-op (test-op "eggquilibrium/tests"))))

(defun should-compress ()
  (when (uiop:getenv "ASDF_COMPRESS")
    t))

#+sb-core-compression (defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
			(uiop:dump-image (asdf:output-file o c) :executable t :compression (should-compress)))

(defsystem "eqquilibrium/tests"
  :author "Sam Kleinman <sam@tychoish.com>"
  :license  "Apache v2"
  :description "test system for eggquilibrium"
  :perform (test-op (op c) (symbol-call :rove :run c))
  :depends-on ("eggquilibrium" "rove")
  :components ((:module "tests"
		:components
		((:file "core")
		 (:file "ext")))))
