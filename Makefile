include vars.mk

#####################
## General targets ##
#####################

.PHONY: all
all: build/${CPPL_NAME}

.PHONY: clean
clean: clean-coreppl

.PHONY: install
install: install-coreppl install-scripts

.PHONY: uninstall
uninstall: uninstall-coreppl uninstall-scripts

.PHONY: test
test: test-coreppl


#############
## CorePPL ##
#############

cppl_tmp_file := $(shell mktemp)
build/${CPPL_NAME}: $(shell find coreppl/src -name "*.mc")
	time mi compile coreppl/src/${CPPL_NAME}.mc --output ${cppl_tmp_file}
	mkdir -p build
	cp ${cppl_tmp_file} build/${CPPL_NAME}
	rm ${cppl_tmp_file}

.PHONY: clean-coreppl
clean-coreppl:
	rm -rf build

# ANSI escape sequence for red text
RED=\033[0;31m
# ANSI escape sequence for resetting text color
RESET=\033[0m
.PHONY: install-coreppl
install-coreppl: build/${CPPL_NAME}
	mkdir -p ${BIN_PATH} ${SRC_PATH};
	cp build/${CPPL_NAME} ${BIN_PATH}/${EXEC_NAME}
	chmod +x ${BIN_PATH}/${EXEC_NAME}
	cp -rf ${CPPL_SRC} ${SRC_PATH}
	@echo "\n${RED}Attention:"
	@echo "${CPPL_NAME} has been installed to ${BIN_PATH} and the CorePPL sources have been installed to ${SRC_PATH}."
	@echo "Please, ensure that the PATH and the MCORE_LIBS environment variables have been set accordingly."
	@echo "E.g. under Bash:"
	@echo 'export PATH=$$PATH:'"${BIN_PATH}"
	@echo 'export MCORE_LIBS=$$MCORE_LIBS:coreppl='"${SRC_PATH}\n${RESET}"

.PHONY: install-cppl
install-cppl:
	mkdir -p ${BIN_PATH}
	cp build/${CPPL_NAME} ${BIN_PATH}/${EXEC_NAME}

.PHONY: uninstall-coreppl
uninstall-coreppl:
	rm -f ${BIN_PATH}/${EXEC_NAME}
	rm -rf ${SRC_PATH}

.PHONY: test-coreppl
test-coreppl: build/${CPPL_NAME}
	@$(MAKE) -s -f test-coreppl.mk all

.PHONY: test-coreppl-compiler
test-coreppl-compiler:
	@$(MAKE) -s -f test-coreppl.mk compiler

.PHONY: test-coreppl-inference
test-coreppl-inference:
	@$(MAKE) -s -f test-coreppl.mk inference

.PHONY: test-coredppl
test-coredppl: build/${CPPL_NAME}
	@$(MAKE) -s -f test-coreppl.mk cdppl
	@$(MAKE) -s -f test-coreppl.mk cdppl-examples

#############
## Scripts ##
#############

.PHONY: install-scripts
install-scripts:
	cp -f scripts/${PLOT_NAME} ${BIN_PATH}/.
	chmod +x ${BIN_PATH}/${PLOT_NAME}

.PHONY: uninstall-scripts
uninstall-scripts:
	rm -f ${BIN_PATH}/${PLOT_NAME}
