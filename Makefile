
.PHONY : all test clean install

all:

test:
	@$(MAKE) -s -f test.mk all

install:
	@cp -f shell/* ~/.local/bin/.
	@mkdir -p ~/.local/lib/midppl/coreppl
	@cp -f coreppl/* ~/.local/lib/midppl/coreppl/.
	@mkdir -p ~/.local/lib/midppl/rootppl
	@cp -f rootppl/*.mc ~/.local/lib/midppl/rootppl/.
clean:
	@rm -f coreppl/*~

