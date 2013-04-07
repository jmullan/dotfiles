PWD = $(shell pwd)

all: install

submodules:
	git submodules init
	git submodules upgrade

portable/.emacs.d/themes/color-theme-solarized: submodules

portable/.dircolors: submodules
	cp submodules/dircolors-solarized/dircolors.ansi-dark portable/.dircolors

submodules/dircolors-solarized:
	git clone git://github.com/seebi/dircolors-solarized.git

install: portable/.dircolors portable/.emacs.d/themes/color-theme-solarized
	sh meta/bin/install.sh $(PWD)
	emacs -batch -f batch-byte-compile portable/lib/emacs/lisp/*.el >/dev/null 2>&1

.phony: install submodules
