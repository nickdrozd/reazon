.PHONY : test

test:
	emacs -Q --batch --directory . -l reazon-tests.el -f ert-run-tests-batch-and-exit
