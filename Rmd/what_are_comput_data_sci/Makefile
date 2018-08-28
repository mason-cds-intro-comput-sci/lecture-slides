include ../../make.inc

MODULE_NAME	=	$(notdir $(patsubst %/,%,$(CURDIR)))

XARINGAN	=	$(OUTPUT_DIR)/01_science.html									\
				$(OUTPUT_DIR)/02_data_overview.html								\
				$(OUTPUT_DIR)/03_data_sampling.html								\
				$(OUTPUT_DIR)/04_computation.html

all : $(OUTPUT_DIR) $(ALL_FILES)

clean :
	$(call cleanup)

$(OUTPUT_DIR) :
	$(call setup_build_directory)

$(XARINGAN) : $(XARINGAN_EXT) | $(OUTPUT_DIR)
	$(call rmarkdown_render,$(XARINGAN_OUTPUT))
