.PHONY: all

# Include test/**/*.mc files but ignore test.mc
test-files=$(shell find coreppl/test -name "*.mc")
test-files := $(filter-out coreppl/test/test.mc,$(test-files))

all: ${test-files}

${test-files}::
	@./make test-cppl $@

