.PHONY: all

test-files=$(shell find coreppl/src -name "*.mc")

all: ${test-files}

${test-files}::
	@boot eval --test $@
