.PHONY: all

test-files=
test-files+=${wildcard coreppl/*.mc}
test-files+=${wildcard coreppl/coreppl-to-mexpr/*.mc}
test-files+=${wildcard coreppl/coreppl-to-rootppl/*.mc}

all: ${test-files}

${test-files}::
	@boot eval --test $@
