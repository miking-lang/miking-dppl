.PHONY : all test test-cppl clean install uninstall

# ANSI escape sequence for red text
RED=\033[0;31m
# ANSI escape sequence for resetting text color
RESET=\033[0m

include vars.mk

all: build/${cppl_name}

cppl_tmp_file := $(shell mktemp)
build/${cppl_name}: $(shell find coreppl/src -name "*.mc")
	time mi compile coreppl/src/${cppl_name}.mc --output ${cppl_tmp_file}
	mkdir -p build
	cp ${cppl_tmp_file} build/${cppl_name}
	rm ${cppl_tmp_file}

install: build/${cppl_name}
# CorePPL
	mkdir -p $(bin_path) $(src_path);
	cp build/${cppl_name} ${bin_path}/${exec_name}
	chmod +x ${bin_path}/${exec_name}
	cp -rf $(cppl_src) $(src_path)

# Scripts
	cp -f scripts/${plot_name} ${bin_path}/.
	chmod +x ${bin_path}/${plot_name}

# RootPPL
	mkdir -p $(dir $(rootppl_bin_path)) $(rootppl_src_path);
	cp -f $(rootppl_bin) $(rootppl_bin_path)
	cp -rf $(rootppl_src) $(rootppl_src_path)

# Information message
	@echo "\n${RED}Attention:"
	@echo "${cppl_name} has been installed to ${bin_path} and the CorePPL sources have been installed to $(src_path)."
	@echo "Please, ensure that the PATH and the MCORE_LIBS environment variables have been set accordingly."
	@echo "E.g. under Bash:"
	@echo 'export PATH=$$PATH:'"${bin_path}"
	@echo 'export MCORE_LIBS=$$MCORE_LIBS:coreppl='"$(src_path)\n${RESET}"

uninstall:
# CorePPL
	rm -f ${bin_path}/${exec_name}
	rm -rf $(src_path)

# Scripts
	rm -f ${bin_path}/${plot_name}

# RootPPL
	rm -rf $(rootppl_bin_path)
	rm -rf $(rootppl_src_path)

clean:
	rm -rf build

test-all: test test-cppl

test:
	@$(MAKE) -s -f test.mk all

test-cppl: build/${cppl_name}
	@$(MAKE) -s -f test-cppl.mk all

install-rootppl:
	mkdir -p $(dir $(rootppl_bin_path)) $(rootppl_src_path);
	cp -f $(rootppl_bin) $(rootppl_bin_path)
	cp -rf $(rootppl_src) $(rootppl_src_path)

uninstall-rootppl:
	rm -rf $(rootppl_bin_path)
	rm -rf $(rootppl_src_path)
