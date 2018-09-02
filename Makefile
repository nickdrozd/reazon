PKGDIR = .
LISPDIR = $(PKGDIR)/
TESTDIR = $(PKGDIR)/test

LISP = $(wildcard $(LISPDIR)/*.el)
TEST = $(wildcard $(TESTDIR)/*.el)
OBJS = $(LISP:.el=.elc) $(TEST:.el=.elc)

# Emacs CLI flags
DIRECTORY = --directory
FUNCALL = --funcall
LOAD = --load

# Emacs batch functions
BATCH = emacs -Q --batch $(DIRECTORY) $(LISPDIR)

COMPILE = $(FUNCALL) batch-byte-compile
RUN_ERT = $(FUNCALL) ert-run-tests-batch-and-exit

# Test commands
RUN_TEST = $(BATCH) $(DIRECTORY) $(TESTDIR)

.PHONY: all clean compile test $(TEST)

all: clean compile test

clean:
	$(RM) $(OBJS)

# This is here to make sure the byte compiler doesn't complain about
# anything, but I don't think it loads any compiled code.
compile:
	$(BATCH) $(COMPILE) $(LISP)
	$(RUN_TEST) $(COMPILE) $(TEST)

test: $(TEST)

# This also runs the test utils file, which contains no tests. This
# doesn't have any ill effects, I don't think, except for the useless
# message "Ran 0 tests, 0 results as expected". Is there a simple way
# to exclude that file?
$(TEST):
	$(RUN_TEST) $(LOAD) $@ $(RUN_ERT)
