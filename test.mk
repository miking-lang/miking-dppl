.PHONY: all

test-files=
test-files+=${wildcard coreppl/*.mc}
test-files+=${wildcard models/*.mc}
test-files+=${wildcard rootppl/*.mc}

all: ${test-files}

${test-files}::
	-@boot eval --test $@
