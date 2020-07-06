(defsystem "eggqulibrium"
  :description "Eggqulibrium helps you use eggs better"
  :author "Sam Kleinman <sam@tychoish.com>"
  :license  "Apache v2"
  :version "0.0.1"
  :depends-on ("cl-csv" "defmain")
  :build-operation "program-op" ;; leave as is
  :build-pathname "build/eggqulibrium"
  :entry-point "eqqulibrium.operations:main"
  :components ((:module "lisp"
		:components
		((:file "model")
		 (:file "parser")
		 (:file "operations"))))
  :in-order-to ((test-op (test-op "eggqulibrium/tests"))))

(defsystem "eqqulibrium/tests"
  :author "Sam Kleinman <sam@tychoish.com>"
  :license  "Apache v2"
  :description "test system for eggqulibrium"
  :perform (test-op (op c) (symbol-call :rove :run c))
  :depends-on ("eggqulibrium" "rove")
  :components ((:module "tests"
		:components
		((:file "core")
		 (:file "ext")))))
