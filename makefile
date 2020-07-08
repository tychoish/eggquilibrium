project := eggqulibrium
directory := lisp

src := $(shell find $(directory) -name "*.lisp")

test:
	sbcl --disable-debugger --eval '(asdf:test-system :$(project))' --quit
build:build/$(project)

build/$(project):$(src)
	sbcl --disable-debugger --eval '(asdf:make :$(project))' --quit

coverage:
	COVERAGE=1 rove $(project).asd

.PHONY:test build coverage
