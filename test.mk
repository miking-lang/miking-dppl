.PHONY: all

test-files=
test-files+=${wildcard coreppl/*.mc}
test-files+=${wildcard rootppl/*.mc}

# NOTE(dlunde,2021-10-27): coreppl/pgm.mc cannot yet be compiled
test-files := $(filter-out coreppl/pgm.mc,$(test-files))

all: ${test-files}

# TODO(dlunde,2021-10-25): We are getting lots of name collisions with `mi`, hence the temp dir. Could be solved by having an `-o` flag for `mi`
${test-files}::
	@mkdir test-$(subst /,-,$@)
	@cd test-$(subst /,-,$@) && ../make test ../$@
	@rmdir test-$(subst /,-,$@)

