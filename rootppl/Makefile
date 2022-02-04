
bin_path = $(HOME)/.local/bin/
src_path=$(HOME)/.local/src/rootppl/

rootppl_name = rootppl

install:
	mkdir -p $(bin_path) $(src_path)
	cp -f $(rootppl_name) $(bin_path)/$(rootppl_name)
	chmod +x $(bin_path)/$(rootppl_name)
	cp -rfT src/ $(src_path)
	cp build.mk $(src_path)
	chmod +x $(src_path)build.mk

uninstall:
	rm -rf $(bin_path)$(rootppl_name)
	rm -rf $(src_path)
