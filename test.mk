.PHONY: all

test-files=
test-files+=${wildcard coreppl/*.mc}
test-files+=${wildcard coreppl/coreppl-to-mexpr/*.mc}
test-files+=${wildcard coreppl/coreppl-to-rootppl/*.mc}

# NOTE(dlunde,2021-10-27): Cannot yet be compiled
test-files := $(filter-out coreppl/pgm.mc,$(test-files))
test-files := $(filter-out coreppl/transformation.mc,$(test-files))

all: ${test-files}

${test-files}::
	@./make test $@

