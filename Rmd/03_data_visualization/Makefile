include ../../make.inc

MODULE_NAME	=	$(notdir $(patsubst %/,%,$(CURDIR)))

XARINGAN	=	$(OUTPUT_DIR)/01_data_visualization_as_communication.html		\
				$(OUTPUT_DIR)/02_qplot_intro.html		\
				$(OUTPUT_DIR)/03_interpreting_scatter_plots.html \
				$(OUTPUT_DIR)/04_plots_of_one_variable.html \
				$(OUTPUT_DIR)/05_interpreting_histograms.html

all : $(OUTPUT_DIR) $(ALL_FILES)

clean :
	$(call cleanup)

$(OUTPUT_DIR) :
	$(call setup_build_directory)

$(XARINGAN) : $(XARINGAN_EXT) | $(OUTPUT_DIR)
	$(call rmarkdown_render,$(XARINGAN_OUTPUT))
