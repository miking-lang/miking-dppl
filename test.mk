.PHONY: all

# Include all .mc files but ignore files under coreppl-to-mexpr that starts
# with "runtime" (they cannot be executed standalone)
test-files=$(shell find coreppl/src -name "*.mc" -not \( -path "*coreppl-to-mexpr/*" -name "runtime*.mc" \))

# NOTE(dlunde,2021-10-27): Cannot yet be compiled
test-files := $(filter-out coreppl/src/pgm.mc,$(test-files))
test-files := $(filter-out coreppl/src/transformation.mc,$(test-files))

all: ${test-files}

${test-files}::
	@./make test $@

