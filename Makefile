PWD = $(shell pwd)

all: install

submodules:
	git submodule sync
	git submodule update --recursive
	git submodule update --init --recursive
	git submodule foreach git pull origin master

portable/.dircolors: submodules
	cp submodules/dircolors-solarized/dircolors.ansi-dark portable/.dircolors

install: submodules portable/.dircolors portable/bin/back
	sh meta/bin/install.sh $(PWD)
	rpl 's-base03    "#002b36"' 's-base03    "#000000"' portable/.emacs.d/elpa/solarized-theme-20180621.1407/solarized.el
	emacs -batch -f batch-byte-compile portable/lib/emacs/lisp/*.el >/dev/null 2>&1 || true

.PHONY: install submodules
