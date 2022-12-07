tppl_name=tpplc
bin_path=${HOME}/.local/bin
src_path=${HOME}/.local/src/cppl/

tppl_tmp_file := $(shell mktemp)
build/${tppl_name}: $(shell find . -name "*.mc" -o -name "*.syn")
	mi compile $(MCORE_STDLIB)/parser/tool.mc
	./tool src/treeppl.syn src/treeppl-to-coreppl/treeppl-ast.mc
	time mi compile src/${tppl_name}.mc --output ${tppl_tmp_file}
	mkdir -p build
	cp ${tppl_tmp_file} build/${tppl_name}
	rm ${tppl_tmp_file}

install: build/${tppl_name}
	cp build/${tppl_name} ${bin_path}/${tppl_name}
	chmod +x ${bin_path}/${tppl_name}

uninstall:
	rm ${bin_path}/${tppl_name}

test: src/treeppl-to-coreppl/compile.mc
	mi run src/treeppl-to-coreppl/compile.mc --test

