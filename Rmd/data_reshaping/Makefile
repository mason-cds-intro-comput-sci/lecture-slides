include ../../make.inc

MODULE_NAME	=	$(notdir $(patsubst %/,%,$(CURDIR)))

XARINGAN	=	$(OUTPUT_DIR)/01_principles_of_tidy_data.html					\
				$(OUTPUT_DIR)/02_pivot_longer.html									\
				$(OUTPUT_DIR)/03_pivot_wider.html									\
				$(OUTPUT_DIR)/04_separate.html									\
				$(OUTPUT_DIR)/05_unite.html

all : $(OUTPUT_DIR) $(ALL_FILES)

clean :
	$(call cleanup)

$(OUTPUT_DIR) :
	$(call setup_build_directory)

$(XARINGAN) : $(XARINGAN_EXT) | $(OUTPUT_DIR)
	$(call rmarkdown_render,$(XARINGAN_OUTPUT))
