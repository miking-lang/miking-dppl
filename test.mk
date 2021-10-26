.PHONY: all

test-files=
test-files+=${wildcard coreppl/*.mc}
test-files+=${wildcard rootppl/*.mc}

all: ${test-files}

# TODO(dlunde,2021-10-25): We are getting lots of name collisions with `mi`, hence the temp dir. Could be solved by having an `-o` flag for `mi`
${test-files}::
	mkdir test_tmp
	cd test_tmp && ../make test ../$@
	rmdir test_tmp

