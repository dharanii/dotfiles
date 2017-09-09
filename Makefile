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
	yaourt -Syua --noconfirm
	yaourt -S --needed --noconfirm `cat packages`
	
	mkdir -p "${HOME}/Pictures/Wallpapers"
	curl -o "${HOME}/Pictures/Wallpapers/graffiti.jpg" \
		https://wallpaperscraft.com/image/graffiti_wall_city_colorful_62146_3840x2160.jpg 
	
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	
	wal -i "${HOME}/Pictures/Wallpapers"

.PHONY: update
update:
	git pull origin master

.PHONY: install
install: update deploy init

.PHONY: backup
backup:
	comm -23 <(pacman -Qeq | sort) <(pacman -Qgq base base-devel | sort) \
		| grep -vx "`cat packages_ignore`" | tee packages

.PHONY: restore
restore:
	yaourt -S --needed --noconfirm `cat packages`
