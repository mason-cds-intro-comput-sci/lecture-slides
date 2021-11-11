include ../../make.inc

MODULE_NAME	=	$(notdir $(patsubst %/,%,$(CURDIR)))

XARINGAN	=	$(OUTPUT_DIR)/01_data_visualization_as_communication.html		\
				$(OUTPUT_DIR)/01_basic_plots.html		\
				$(OUTPUT_DIR)/02_examining_numerical_data.html					\
				$(OUTPUT_DIR)/03_describing_visualizations.html					\
				$(OUTPUT_DIR)/04_data_visualization_with_ggplot2.html

all : $(OUTPUT_DIR) $(ALL_FILES)

clean :
	$(call cleanup)

$(OUTPUT_DIR) :
	$(call setup_build_directory)

$(XARINGAN) : $(XARINGAN_EXT) | $(OUTPUT_DIR)
	$(call rmarkdown_render,$(XARINGAN_OUTPUT))
