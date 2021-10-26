
.PHONY : all test test-boot clean install

all:

test:
	@$(MAKE) -s -f test.mk all

test-boot:
	@$(MAKE) -s -f test-boot.mk all

install:
	@cp -f shell/midppl ~/.local/bin/.
	@mkdir -p ~/.local/lib/midppl/coreppl
	@cp -f coreppl/* ~/.local/lib/midppl/coreppl/.
	@mkdir -p ~/.local/lib/midppl/rootppl
	@cp -f rootppl/*.mc ~/.local/lib/midppl/rootppl/.

uninstall:
	@rm -rf ~/.local/bin/midppl
	@rm -rf ~/.local/lib/midppl

clean:
	@rm -f coreppl/*~

