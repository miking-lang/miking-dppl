include vars.mk

# Include all .mc files but ignore files under coreppl-to-mexpr that starts
# with "runtime" (they cannot be executed standalone)
test-files=$(shell find coreppl/src -name "*.mc" -not \( -path "*coreppl-to-mexpr/*" \( -name "runtime*.mc" -or -name "top.mc" \) \))
test-files+=coreppl/src/coreppl-to-mexpr/runtimes.mc

# NOTE(dlunde,2021-10-27): Cannot yet be compiled
test-files := $(filter-out coreppl/src/pgm.mc,$(test-files))
test-files := $(filter-out coreppl/src/transformation.mc,$(test-files))


test-infer-files=$(shell find coreppl/test/coreppl-to-mexpr/infer -name "*.mc")
test-cli-files=\
  $(shell find coreppl/test/coreppl-to-mexpr/cli \
               coreppl/test/coreppl-to-rootppl/cli -name "*.mc")

.PHONY: all
all: compiler cppl


####################################
## Tests in compiler source files ##
####################################

.PHONY: compiler
compiler: ${test-files}

${test-files}::
	@./make test $@


###################
## CorePPL tests ##
###################

.PHONY: cppl
cppl: ${test-infer-files} ${test-cli-files}

export CPPL_NAME
export MIDPPL_PATH=${CURDIR}
export MIDPPL_SRC=${MIDPPL_PATH}/${CPPL_SRC}

export ROOTPPL_BIN
export RPPL_ENGINE_SRC=${MIDPPL_PATH}/${ROOTPPL_SRC}

# Infer tests
${test-infer-files}::
	@./make test-cppl $@ "build/${CPPL_NAME}"

# CLI tests
${test-cli-files}::
	@./make test $@
