PWD = $(shell pwd)
install:
	sh meta/bin/install.sh $(PWD)
.phony: install