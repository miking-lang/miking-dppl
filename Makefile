





.PHONY : all test clean


all:

test:
	boot run --test coreppl

clean:
	rm -f coreppl/*~

