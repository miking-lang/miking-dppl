
.PHONY : all test clean install

all:

test:
	@$(MAKE) -s -f test.mk all

install:
	@cp -f shell/* ~/.local/bin/.
	@mkdir -p ~/.local/lib/midppl
	@cp -f coreppl/* ~/.local/lib/midppl/.
clean:
	@rm -f coreppl/*~

