# Credit for ROOTDIR implementation:
# kenorb (https://stackoverflow.com/users/55075/kenorb),
# How to get current relative directory of your Makefile?,
# URL (version: 2017-05-23): https://stackoverflow.com/a/35698978
ROOTDIR			=	$(abspath $(patsubst %/,%,$(dir $(abspath $(lastword 		\
					$(MAKEFILE_LIST))))))

SHELL			=	/bin/sh

RM				=	rm
COPY			=	cp
LN				=	ln

R				=	Rscript

MD				=
XARINGAN		=

BUILD_DIR		=	$(ROOTDIR)/build
MODULE_NAME		=
OUTPUT_DIR		=	$(patsubst %/,%,$(BUILD_DIR)/$(MODULE_NAME))

MD_OUTPUT		=	github_document
XARINGAN_OUTPUT	=	xaringan::moon_reader

MD_EXT			=	$(OUTPUT_DIR)/%.md : %.Rmd
XARINGAN_EXT	=	$(OUTPUT_DIR)/%.html : %.Rmd

ALL_FILES		=	$(MD)														\
					$(XARINGAN)

CLEAN_FILES		=	*_files/													\
					*_cache/

define make_all
	@echo All builds in $(CURDIR) complete!
endef

define setup_build_directory
	mkdir -p "$@"
endef

define cleanup
	-$(RM) -rf $(CLEAN_FILES)
	-$(RM) -f $(ALL_FILES)
	-$(RM) -rf $(BUILD_DIR)
endef

define install_deps
	$(R) -e "install.packages('remotes', repos = 'https://cran.rstudio.com')"	\
		 -e "remotes::install_deps()"
endef

define rmarkdown_render
	$(R) -e "rmarkdown::render(input = '$<', output_file = '$@',			\
			 output_format = '$(1)')"
endef
