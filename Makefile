ASSIGNMENT  = cse230-trees
ZIPFILE     = $(ASSIGNMENT).zip
ZIPCONTENTS = img platform src test out $(ASSIGNMENT).cabal stack.yaml Makefile .gitignore LICENSE README.md

STACK       = stack --allow-different-user

.PHONY: all test build clean distclean turnin

# make run silent
.SILENT:

all: test

test: clean
	$(STACK) test --test-arguments="--num-threads 1"

clean:
	rm -f img/*.png out/*.log

distclean: clean
	$(STACK) clean

turnin: clean
	rm -f $(ZIPFILE)
	zip -r $(ZIPFILE) $(ZIPCONTENTS) -x '*/\.*' -x@.gitignore
