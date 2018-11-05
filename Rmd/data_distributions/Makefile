include ../../make.inc

MODULE_NAME	=	$(notdir $(patsubst %/,%,$(CURDIR)))

XARINGAN	=	$(OUTPUT_DIR)/01_quantify_distributions.html					\
				$(OUTPUT_DIR)/02_quantify_distributions_in_R.html				\
				$(OUTPUT_DIR)/03_probability_mass_functions.html				\
				$(OUTPUT_DIR)/04_cumulative_distribution_functions.html

all : $(OUTPUT_DIR) $(ALL_FILES)

clean :
	$(call cleanup)

$(OUTPUT_DIR) :
	$(call setup_build_directory)

$(XARINGAN) : $(XARINGAN_EXT) | $(OUTPUT_DIR)
	$(call rmarkdown_render,$(XARINGAN_OUTPUT))
