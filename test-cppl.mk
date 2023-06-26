.PHONY: all

# Include all .mc files but ignore files under coreppl-to-mexpr that starts
# with "runtime" (they cannot be executed standalone)
test-files=$(shell find coreppl/test -name "*.mc")
test-files := $(filter-out coreppl/test/test.mc,$(test-files))

all: ${test-files}

${test-files}::
	@./make test-cppl $@

