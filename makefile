all: compile test

compile:
	emacs -Q --batch -l center-layout.el

test:
	./test.sh

debug:
	emacs -Q -l center-layout.el --eval "(center-layout-mode)"
