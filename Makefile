
.PHONY : all test test-boot clean install uninstall

main_name=main
exec_name=midppl
plot_name=dplot
bin_path=${HOME}/.local/bin
lib_path=${HOME}/.local/lib
lib_path_coreppl=${lib_path}/mcore/stdlib/coreppl

all: build/${main_name}

build/${main_name}: $(shell find . -name "*.mc")
	time mi compile coreppl/${main_name}.mc
	mkdir -p build
	cp ${main_name} build/${main_name}
	rm ${main_name}

install: build/${main_name}
	cp build/${main_name} ${bin_path}/${exec_name}
	chmod +x ${bin_path}/${exec_name}
	mkdir -p ${lib_path_coreppl}
	cp -f coreppl/* ${lib_path_coreppl}/.
	cp -f scripts/${plot_name} ${bin_path}/.
	chmod +x ${bin_path}/${plot_name}

uninstall:
	rm -f ${bin_path}/${exec_name}
	rm -f ${bin_path}/${plot_name}
	rm -rf ${lib_path_coreppl}

clean:
	rm -rf build
	rm -f coreppl/*~

test:
	@$(MAKE) -s -f test.mk all

test-boot:
	@$(MAKE) -s -f test-boot.mk all
