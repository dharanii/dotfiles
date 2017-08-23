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
