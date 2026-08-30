EMACS ?= emacs

.PHONY: test

test:
	$(EMACS) -Q --batch -L lisp -L test \
		-l test/auto-research-test.el \
		-f ert-run-tests-batch-and-exit
