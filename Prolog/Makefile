.DEFAULT_GOAL := help
ROOT:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

# PROLOG PATH
PROLOG:=$(shell which swipl)

# Colors
GRAY="\033[1;30m"
RED="\033[1;31m"
GREEN="\033[32m"
HGREEN="\033[1;32m"
YELLOW="\033[1;33m"
LYELLOW="\033[33m"
BLUE="\033[1;34m"
LBLUE="\033[34m"
PINK="\033[35m"
CYAN="\033[1;36m"
WHITE="\033[0;37m"
NOCOLOR="\033[0m\033[K"

# Utils
define logInfo
	@printf $(GREEN)"${1}"$(NOCOLOR)"\n"
endef

define logValue
	@printf $(LYELLOW)"${1}: "$(WHITE)"${2}"$(NOCOLOR)"\n"
endef


help: ## Display this help message
	@echo
	@printf $(HGREEN)=$(BLUE)-------------------------------------------------$(HGREEN)=$(NOCOLOR)"\n"
	@printf $(HGREEN)="                     Prolog                      "=$(NOCOLOR)"\n"
	@printf $(HGREEN)=$(BLUE)-------------------------------------------------$(HGREEN)=$(NOCOLOR)"\n"
	@echo
	@printf $(LYELLOW)"Please use \`make <target>\` where <target> is one of\n\n"$(NOCOLOR)
	@grep -E '(^[0-9a-zA-Z_-]+:.*?##.*$$)|(^##)' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf $(GREEN)"%-25s"$(NOCOLOR)"%s\n", $$1, $$2}' | sed -e "s/\[32m##/[36m/"

.PHONY: help


##
##Section:
##


prolog: ## Run swipl with *.pl file initialized (FILE?=oop)
prolog: FILE?=oop
prolog:
	@if [ -f ${FILE}.pl ]; then \
		printf $(GREEN)"File ${FILE}.pl found. Running Prolog...\n"$(NOCOLOR); \
		$(PROLOG) -s ${FILE}.pl; \
	else \
		echo $(RED)"\nFile ${FILE}.pl not found. Exiting...\n"$(NOCOLOR); \
	fi

.PHONY: prolog

##
##