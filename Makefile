





.PHONY : all test clean


all:

test:
	@boot eval --test coreppl

clean:
	@rm -f coreppl/*~

