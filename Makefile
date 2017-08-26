SHELL := /bin/bash

DOTPATH    := $(PWD)
CANDIDATES := $(wildcard .??*) bin
EXCLUSIONS := .DS_Store .git
DOTFILES   := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

.PHONY: deploy
deploy:
		@$(foreach val, $(DOTFILES), ln -sfnv $(abspath $(val)) $(HOME)/$(val);)

.PHONY: init
init:
		# @$(foreach val, $(wildcard ./etc/init/*.sh), bash $(val);)
		sh init

.PHONY: update
update:
		git pull origin master

.PHONY: install
install: update deploy init

.PHONY: backup
backup:
		comm -23 <(pacman -Qeq | sort) <(pacman -Qgq base base-devel | sort) > ./packages.txt

