EMACS ?= emacs

.PHONY: test clean

# Byte-compile with warnings as errors, run checkdoc, then the ERT suites on
# the source and on the compiled code.
test:
	EMACS="$(EMACS)" test/run-tests.sh

clean:
	rm -f *.elc test/*.elc
