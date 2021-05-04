
.PHONY : all test clean

all:

test:
	@$(MAKE) -s -f test.mk all

clean:
	@rm -f coreppl/*~

