CPPL_NAME=cppl
PLOT_NAME=dppl-plot
BIN_PATH=${HOME}/.local/bin
SRC_PATH=${HOME}/.local/src/coreppl/
CPPL_SRC=coreppl/src/.

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

build/${CPPL_NAME}: $(shell find coreppl/src -name "*.mc")
	mkdir -p build
	mi compile coreppl/src/${CPPL_NAME}.mc --output $@

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

misc/test: misc/test-spec.mc
	mi compile $< --output $@

.PHONY: test-coreppl
test-coreppl: misc/test
	+misc/test

.PHONY: test-coreppl-inference
test-coreppl-inference: misc/test
	+misc/test --all-dep `find coreppl/test/coreppl-to-mexpr/inference-accuracy -name '*.mc'`

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
