.PHONY: all

test-files=
test-files+=${wildcard lib/coreppl/*.mc}
test-files+=${wildcard lib/rootppl/*.mc}
test-files+=${wildcard lib/mexpr/*.mc}

# NOTE(dlunde,2021-10-27): coreppl/pgm.mc cannot yet be compiled
test-files := $(filter-out lib/coreppl/pgm.mc,$(test-files))

all: ${test-files}

${test-files}::
	@./make test $@

