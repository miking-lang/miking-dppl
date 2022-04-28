.PHONY: all

test-files=
test-files+=${wildcard src/*.mc}
test-files+=${wildcard src/coreppl/*.mc}
test-files+=${wildcard src/mexpr/*.mc}
test-files+=${wildcard src/rootppl/*.mc}

all: ${test-files}

${test-files}::
	@boot eval --test $@
