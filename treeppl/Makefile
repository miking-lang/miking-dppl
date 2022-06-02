tppl_name=tpplc
bin_path=${HOME}/.local/bin
src_path=${HOME}/.local/src/cppl/

tppl_tmp_file := $(shell mktemp)
build/${tppl_name}: $(shell find . -name "*.mc" -o -name "*.syn")
	mi compile $(MCORE_STDLIB)/parser/tool.mc
	./tool src/treeppl.syn src/treeppl-ast.mc
	time mi compile src/${tppl_name}.mc --output ${tppl_tmp_file}
	mkdir -p build
	cp ${tppl_tmp_file} build/${tppl_name}
	rm ${tppl_tmp_file}

install: build/${tppl_name}
	cp build/${tppl_name} ${bin_path}/${tppl_name}
	chmod +x ${bin_path}/${tppl_name}

example-flip: build/${tppl_name} models/flip/flip.tppl models/flip/flip-io.mc
	build/${tppl_name} models/flip/flip.tppl models/flip/flip-io.mc

example-if: build/${tppl_name} models/iftest/if.tppl
	build/${tppl_name} models/iftest/simple-if.tppl models/flip/flip-io.mc

example-simple-if: build/${tppl_name} models/iftest/simple-if.tppl
	#boot eval --debug-stack-trace src/tpplc.mc  -- models/iftest/simple-if.tppl  models/flip/flip-io.mc
	boot eval --debug-stack-trace src/testparser.mc  -- models/iftest/simple-if.tppl

example-crbd: build/${tppl_name} models/crbd/crbd-demo.tppl
	boot eval --debug-stack-trace src/testparser.mc -- models/crbd/crbd-demo.tppl

all: example-flip example-if

.PHONY: example-flip install-treeppl example-if
