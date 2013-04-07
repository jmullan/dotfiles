PWD = $(shell pwd)

all: install

submodules:
	git submodules init
	git submodules upgrade

portable/.emacs.d/themes/color-theme-solarized: submodules

portable/.dircolors: submodules
	cp submodules/dircolors-solarized/dircolors.ansi-dark portable/.dircolors

portable/bin/back: submodules
	cp submodules/back/back portable/bin/back

install: portable/.dircolors portable/.emacs.d/themes/color-theme-solarized portable/bin/back
	sh meta/bin/install.sh $(PWD)
	emacs -batch -f batch-byte-compile portable/lib/emacs/lisp/*.el >/dev/null 2>&1

.phony: install submodules
