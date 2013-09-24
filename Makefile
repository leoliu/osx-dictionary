.PHONY: all clean test

ELCFILES = $(addsuffix .elc, $(basename $(wildcard *.el)))

test: $(ELCFILES)
	@make -C test

%.elc : %.el
	@echo Compiling $<
	@emacs -batch -q -no-site-file -f batch-byte-compile $<

clean:
	@rm -f *.elc
