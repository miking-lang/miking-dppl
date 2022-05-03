.PHONY: all

test-files=
test-files+=${wildcard coreppl/*.mc}
test-files+=${wildcard coreppl/mexpr/*.mc}
test-files+=${wildcard coreppl/rootppl/*.mc}
test-files+=${wildcard midppl/*.mc}

all: ${test-files}

${test-files}::
	@boot eval --test $@
