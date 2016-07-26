PWD = $(shell pwd)

all: install

submodules:
	git submodule sync
	git submodule update --recursive
	git submodule update --init --recursive
	git submodule foreach git pull origin master

portable/.emacs.d/themes/color-theme-solarized: submodules

portable/.dircolors: submodules
	cp submodules/dircolors-solarized/dircolors.ansi-dark portable/.dircolors

install: submodules portable/.dircolors portable/.emacs.d/themes/color-theme-solarized portable/bin/back
	sh meta/bin/install.sh $(PWD)
	emacs -batch -f batch-byte-compile portable/lib/emacs/lisp/*.el >/dev/null 2>&1 || true

.PHONY: install submodules
