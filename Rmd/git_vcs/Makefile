include ../../make.inc

MODULE_NAME	=	$(notdir $(patsubst %/,%,$(CURDIR)))

XARINGAN	=	$(OUTPUT_DIR)/01_why_git.html \
            $(OUTPUT_DIR)/02_commits.html \
            $(OUTPUT_DIR)/03_branches.html \
            $(OUTPUT_DIR)/04_remotes.html \
            $(OUTPUT_DIR)/05_rstudio_git.html \
            $(OUTPUT_DIR)/06_problems_and_pitfalls.html

all : $(OUTPUT_DIR) $(ALL_FILES)

clean :
	$(call cleanup)

$(OUTPUT_DIR) :
	$(call setup_build_directory)

$(XARINGAN) : $(XARINGAN_EXT) | $(OUTPUT_DIR)
	$(call rmarkdown_render,$(XARINGAN_OUTPUT))
