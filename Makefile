PWD = $(shell pwd)

all: install

submodules:
	git submodule update --recursive
	git submodule update --init --recursive

portable/.emacs.d/themes/color-theme-solarized: submodules

portable/.dircolors: submodules
	cp submodules/dircolors-solarized/dircolors.ansi-dark portable/.dircolors

install: submodules portable/.dircolors portable/.emacs.d/themes/color-theme-solarized portable/bin/back
	sh meta/bin/install.sh $(PWD)
	emacs -batch -f batch-byte-compile submodules/js3-mode/*.el >/dev/null 2>&1
	emacs -batch -f batch-byte-compile portable/lib/emacs/lisp/*.el >/dev/null 2>&1

.PHONY: install submodules
