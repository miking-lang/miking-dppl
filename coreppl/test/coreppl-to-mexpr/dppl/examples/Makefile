ROOT := $(shell git rev-parse --show-toplevel)
REL  := $(shell realpath --relative-to=$(ROOT) $(CURDIR))

.PHONY: plots
plots:
	python plot.py "$(ROOT)/build/$(REL)"

.PHONY: clean
clean:
	rm -rf *.pdf
