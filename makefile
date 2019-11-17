all: compile test

compile:
	emacs -Q --batch -l center-layout.el

test:
	emacs -Q --batch -l center-layout.el -l center-layout-test.el -f ert-run-tests-batch-and-exit

debug:
	emacs -Q -l center-layout.el --eval "(center-layout-mode)"
