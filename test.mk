.PHONY: all

test-files=
test-files+=${wildcard coreppl/*.mc}
test-files+=${wildcard rootppl/*.mc}

# NOTE(dlunde,2021-10-27): coreppl/pgm.mc cannot yet be compiled
test-files := $(filter-out coreppl/pgm.mc coreppl/main.mc,$(test-files))

all: ${test-files}

${test-files}::
	@./make test $@

