.PHONY: all

include vars.mk

# Include test/coreppl-to-mexpr/*.mc files
test-mexpr-files=coreppl/test/coreppl-to-mexpr.mc
test-rootppl-files=coreppl/test/coreppl-to-rootppl.mc

all: mexpr rootppl

mexpr: ${test-mexpr-files}
rootppl: ${test-rootppl-files}

# Tests for the MExpr backend
export CPPL_NAME
export MIDPPL_PATH=${CURDIR}
export MIDPPL_SRC=${MIDPPL_PATH}/${CPPL_SRC}
${test-mexpr-files}::
	@./make test-cppl $@ "build/${CPPL_NAME}"

# Tests for the RootPPL backend
${test-rootppl-files}::
	@./make test $@
