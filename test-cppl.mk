.PHONY: all

include vars.mk

# Include test/coreppl-to-mexpr/*.mc files
test-mexpr-files=coreppl/test/coreppl-to-mexpr.mc
test-rootppl-files=coreppl/test/coreppl-to-rootppl.mc

all: mexpr rootppl

mexpr: ${test-mexpr-files}
rootppl: ${test-rootppl-files}

${test-mexpr-files}::
	@./make test-cppl $@ "build/${cppl_name}"

${test-rootppl-files}::
	@./make test $@
