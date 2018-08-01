SRCS = $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)

BATCH = emacs -Q --batch --directory .

.PHONY: clean compile test

clean:
	$(RM) $(OBJS)

compile:
	$(BATCH) -f batch-byte-compile $(SRCS)

test:
	$(BATCH) -l reazon-tests.el -f ert-run-tests-batch-and-exit
