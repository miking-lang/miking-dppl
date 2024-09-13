include vars.mk

# Include all .mc files but ignore files under coreppl-to-mexpr that starts
# with "runtime" (they cannot be executed standalone)
test-files=$(shell find coreppl/src -name "*.mc" -not \( -path "*coreppl-to-mexpr/*" \( -name "runtime*.mc" -or -name "top.mc" \) \))
test-files+=coreppl/src/coreppl-to-mexpr/runtimes.mc

# NOTE(dlunde,2021-10-27): Cannot yet be compiled
test-files := $(filter-out coreppl/src/pgm.mc,$(test-files))
test-files := $(filter-out coreppl/src/transformation.mc,$(test-files))

# NOTE(oerikss, 2024-04-08): Filter out the main file as it it print to standard
# out and it is compiled anyways when doing the inference tests.
test-files := $(filter-out coreppl/src/cppl.mc,$(test-files))

test-infer-files=$(shell find coreppl/test/coreppl-to-mexpr/infer -name "*.mc")
test-staticdelay-files=$(shell find coreppl/test/coreppl-to-mexpr/static-delay -name "*.mc")
test-cli-files=\
  $(shell find coreppl/test/coreppl-to-mexpr/cli -name "*.mc")
# NOTE(johnwikman, 2024-09-13): The RootPPL compilation is currently broken, so
# leaving it commented out until someone has fixed RootPPL compilation.
#test-cli-files+=\
#  $(shell find coreppl/test/coreppl-to-rootppl/cli -name "*.mc")

.PHONY: all
all: compiler cppl


############################################
## Tests in CorePPL compiler source files ##
############################################

.PHONY: compiler
compiler: ${test-files}

${test-files}::
	@./make test $@

###################
## CorePPL tests ##
###################

.PHONY: cppl
cppl: ${test-staticdelay-files} ${test-infer-files} ${test-cli-files} 

.PHONY: static-delay
static-delay: ${test-staticdelay-files}

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

# Static delay tests
${test-staticdelay-files}::
	@./make test $@
