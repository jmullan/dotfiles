PWD = $(shell pwd)

all: install

submodules: portable/bin/back portable/bin/git-helpers portable/bin/git-toolbelt portable/bin/gitflow
	git submodule sync
	git submodule update --init --recursive
	git submodule update --recursive
	git submodule foreach git fetch
	git submodule foreach 'git checkout main 2>/dev/null || git checkout master'
	git submodule foreach git rebase

portable/.dircolors: submodules
	cp submodules/dircolors-solarized/dircolors.ansi-dark portable/.dircolors

install: submodules portable/.dircolors portable/bin/back
	find . -name '*.bak' -delete
	sh meta/bin/install.sh $(PWD)
	rpl 's-base03    "#002b36"' 's-base03    "#000000"' portable/.emacs.d/elpa/solarized-theme-20180621.1407/solarized.el
	emacs -batch -f batch-byte-compile portable/lib/emacs/lisp/*.el >/dev/null 2>&1 || true
	portable/bin/update_dotfiles_venv

portable/bin/back:
	ln -s '../../submodules/back' 'portable/bin/back'

portable/bin/git-helpers:
	ln -s '../../submodules/git-helpers' 'portable/bin/git-helpers'

portable/bin/git-toolbelt:
	ln -s '../../submodules/git-toolbelt' 'portable/bin/git-toolbelt'

portable/bin/gitflow:
	ln -s '../../submodules/gitflow' 'portable/bin/gitflow'


.PHONY: install submodules
