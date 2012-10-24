PWD = $(shell pwd)
install:
	-emacs -batch -f batch-byte-compile portable/lib/emacs/lisp/*.el >/dev/null 2>&1
	sh meta/bin/install.sh $(PWD)

.phony: install
