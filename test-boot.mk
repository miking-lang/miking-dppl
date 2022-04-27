.PHONY: all

test-files=
test-files+=${wildcard lib/coreppl/*.mc}
test-files+=${wildcard lib/rootppl/*.mc}

all: ${test-files}

${test-files}::
	@boot eval --test $@
