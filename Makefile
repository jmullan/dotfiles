PWD = $(shell pwd)

all: install

submodules: portable/bin/back portable/bin/git-helpers portable/bin/git-toolbelt portable/bin/gitflow
	git submodule sync
	git submodule update --init --recursive
	git submodule update --recursive
	git submodule foreach git fetch


portable/.dircolors: submodules
	cp submodules/dircolors-solarized/dircolors.ansi-dark portable/.dircolors

install: submodules portable/.dircolors portable/bin/back
	find . -name '*.bak' -delete
	sh meta/bin/install.sh $(PWD)
	fastreplace 's-base03    "#002b36"' 's-base03    "#000000"' portable/.emacs.d/elpa/solarized-theme-20180621.1407/solarized.el
	emacs -batch -f batch-byte-compile portable/lib/emacs/lisp/*.el >/dev/null 2>&1 || true
	portable/bin/update_dotfiles_venv
	uv tool install 'jmullan.git@git+https://github.com/jmullan/git-helpers'
	uv tool install 'jmullan.artificer@git+https://github.com/jmullan/jmullan.artificer'
	git submodule foreach 'git main --refresh'
	git submodule foreach 'git checkout main 2>/dev/null || git checkout master'
	git submodule foreach git rebase
	locale -a | grep en_US.utf8 || echo "sudo locale-gen en_US.UTF-8"

portable/bin/back:
	ln -s '../../submodules/back' 'portable/bin/back'

portable/bin/git-helpers:
	ln -s '../../submodules/git-helpers' 'portable/bin/git-helpers'

portable/bin/git-toolbelt:
	ln -s '../../submodules/git-toolbelt' 'portable/bin/git-toolbelt'

portable/bin/gitflow:
	ln -s '../../submodules/gitflow' 'portable/bin/gitflow'


.PHONY: install submodules
