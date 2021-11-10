
.PHONY : all test test-boot clean install uninstall

main_name=main
exec_name=midppl
bin_path=${HOME}/.local/bin

all: build/${main_name}

build/${main_name}: $(shell find . -name "*.mc")
	time mi compile coreppl/${main_name}.mc
	mkdir -p build
	cp ${main_name} build/${main_name}
	rm ${main_name}

install: build/${main_name}
	cp build/${main_name} ${bin_path}/${exec_name}
	chmod +x ${bin_path}/${exec_name}

uninstall:
	rm -f ${bin_path}/${exec_name}

clean:
	rm -rf build
	rm -f coreppl/*~

test:
	@$(MAKE) -s -f test.mk all

test-boot:
	@$(MAKE) -s -f test-boot.mk all
