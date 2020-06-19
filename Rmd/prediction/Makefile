include ../../make.inc

MODULE_NAME	=	$(notdir $(patsubst %/,%,$(CURDIR)))

XARINGAN	=	$(OUTPUT_DIR)/01_modeling_for_prediction.html			\
				$(OUTPUT_DIR)/02_data_splits.html         \
				$(OUTPUT_DIR)/03_generalized_linear_models.html

all : $(OUTPUT_DIR) $(ALL_FILES)

clean :
	$(call cleanup)

$(OUTPUT_DIR) :
	$(call setup_build_directory)

$(XARINGAN) : $(XARINGAN_EXT) | $(OUTPUT_DIR)
	$(call rmarkdown_render,$(XARINGAN_OUTPUT))
