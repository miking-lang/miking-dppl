.PHONY: all

test-files=
test-files+=${wildcard src/*.mc}
test-files+=${wildcard src/coreppl/*.mc}
test-files+=${wildcard src/mexpr/*.mc}
test-files+=${wildcard src/rootppl/*.mc}

# NOTE(dlunde,2021-10-27): Cannot yet be compiled
test-files := $(filter-out src/coreppl/pgm.mc,$(test-files))
test-files := $(filter-out src/coreppl/transformation.mc,$(test-files))

all: ${test-files}

${test-files}::
	@./make test $@

