
.PHONY : all test test-boot clean install uninstall

cppl_name=cppl
exec_name=cppl
tppl_name=tpplc
plot_name=dppl-plot
bin_path=${HOME}/.local/bin
src_path=${HOME}/.local/src/coreppl/
cppl_src=coreppl/src/

rootppl_bin_path = $(HOME)/.local/bin/rppl
rootppl_src_path=$(HOME)/.local/src/rootppl/
rootppl_bin = rootppl/rppl
rootppl_src = rootppl/src/

all: build/${cppl_name}

cppl_tmp_file := $(shell mktemp)
build/${cppl_name}: $(shell find . -name "*.mc")
	time mi compile coreppl/src/${cppl_name}.mc --output ${cppl_tmp_file}
	mkdir -p build
	cp ${cppl_tmp_file} build/${cppl_name}
	rm ${cppl_tmp_file}

tppl_tmp_file := $(shell mktemp)
build/${tppl_name}: $(shell find . -name "*.mc" -o -name "*.syn")
	mi compile $(MCORE_STDLIB)/parser/tool.mc
	./tool treeppl/src/treeppl.syn treeppl/src/treeppl-ast.mc
	time mi compile treeppl/src/${tppl_name}.mc --output ${tppl_tmp_file}
	mkdir -p build
	cp ${tppl_tmp_file} build/${tppl_name}
	rm ${tppl_tmp_file}

install: build/${cppl_name}
# CorePPL
	mkdir -p $(bin_path) $(src_path);
	cp build/${cppl_name} ${bin_path}/${exec_name}
	chmod +x ${bin_path}/${exec_name}
	cp -rfT $(cppl_src) $(src_path)

# Scripts
	cp -f scripts/${plot_name} ${bin_path}/.
	chmod +x ${bin_path}/${plot_name}

# RootPPL
	mkdir -p $(dir $(rootppl_bin_path)) $(rootppl_src_path);
	cp -f $(rootppl_bin) $(rootppl_bin_path)
	cp -rfT $(rootppl_src) $(rootppl_src_path)

uninstall:
# CorePPL
	rm -f ${bin_path}/${exec_name}
	rm -rf $(src_path)

# Scripts
	rm -f ${bin_path}/${plot_name}

# RootPPL
	rm -rf $(rootppl_bin_path)
	rm -rf $(rootppl_src_path)


install-treeppl: build/${tppl_name}
# TreePPL
	cp build/${tppl_name} ${bin_path}/${tppl_name}
	chmod +x ${bin_path}/${tppl_name}

uninstall-treeppl:
# TreePPL
	rm -f ${bin_path}/${tppl_name}

clean:
	rm -rf build

test:
	@$(MAKE) -s -f test.mk all

test-boot:
	@$(MAKE) -s -f test-boot.mk all

install-rootppl:
	mkdir -p $(dir $(rootppl_bin_path)) $(rootppl_src_path);
	cp -f $(rootppl_bin) $(rootppl_bin_path)
	cp -rfT $(rootppl_src) $(rootppl_src_path)

uninstall-rootppl:
	rm -rf $(rootppl_bin_path)
	rm -rf $(rootppl_src_path)
