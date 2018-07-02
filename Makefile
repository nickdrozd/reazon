.PHONY : test

test:
	emacs -Q --batch --directory . -l reason-tests.el -f ert-run-tests-batch-and-exit
