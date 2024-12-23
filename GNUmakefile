EMACS ?= emacs
BATCH = $(EMACS) -Q -batch -L .

ELS = privatebin.el
OBJECTS = $(ELS:.el=.elc)

.PHONY: all compile clean test

all: compile

compile: $(OBJECTS)

clean:
	rm -f $(OBJECTS)

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<
