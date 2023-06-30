include vars.mk

#####################
## General targets ##
#####################

.PHONY: all
all: build/${CPPL_NAME}

.PHONY: clean
clean: clean-coreppl

.PHONY: install
install: install-coreppl install-rootppl install-scripts

.PHONY: uninstall
uninstall: uninstall-coreppl uninstall-rootppl uninstall-scripts

.PHONY: test
test: test-coreppl test-rootppl


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

.PHONY: uninstall-coreppl
uninstall-coreppl:
	rm -f ${BIN_PATH}/${EXEC_NAME}
	rm -rf ${SRC_PATH}

.PHONY: test-coreppl
test-coreppl: build/${CPPL_NAME}
	@$(MAKE) -s -f test.mk all


#############
## RootPPL ##
#############

.PHONY: install-rootppl
install-rootppl:
	mkdir -p $(dir ${ROOTPPL_BIN_PATH}) ${ROOTPPL_SRC_PATH};
	sed 's\$$RPPL_ENGINE_SRC\${ROOTPPL_SRC_PATH}\g' ${ROOTPPL_BIN} > ${ROOTPPL_BIN_PATH}
	chmod +x ${ROOTPPL_BIN_PATH}
	cp -rfT ${ROOTPPL_SRC} ${ROOTPPL_SRC_PATH}

.PHONY: uninstall-rootppl
uninstall-rootppl:
	rm -rf ${ROOTPPL_BIN_PATH}
	rm -rf ${ROOTPPL_SRC_PATH}

# TODO(2023-06-28,dlunde): Currently no pure RootPPL tests (although we test
# the RootPPL backend of cppl as part of test-cppl). There seem to be some
# tests under rootppl/, but I do not know their dependencies (seem to be some R
# stuff there, for example) and how to run them from this makefile.
.PHONY: test-rootppl
test-rootppl:


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

