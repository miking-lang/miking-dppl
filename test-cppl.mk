.PHONY: all

include vars.mk

# Include test/coreppl-to-mexpr/*.mc files
test-mexpr-files=coreppl/test/coreppl-to-mexpr.mc
test-rootppl-files=coreppl/test/coreppl-to-rootppl.mc

all: mexpr rootppl

mexpr: ${test-mexpr-files}
rootppl: ${test-rootppl-files}

# Use the CorePPL source files in the current directory for the tests (needed
# for runtime files used by the compiler)
MIDPPL_SRC=${CURDIR}/${CPPL_SRC}


# Tests for the MExpr backend
export CPPL_NAME
${test-mexpr-files}::
	@./make test-cppl $@ "build/${CPPL_NAME}"

# Tests for the RootPPL backend
${test-rootppl-files}::
	@./make test $@
