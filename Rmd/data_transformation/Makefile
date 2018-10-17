include ../../make.inc

MODULE_NAME	=	$(notdir $(patsubst %/,%,$(CURDIR)))

XARINGAN	=	$(OUTPUT_DIR)/01_data_wrangling.html							\
				$(OUTPUT_DIR)/02_presidential.html								\
				$(OUTPUT_DIR)/03_select.html									\
				$(OUTPUT_DIR)/04_pipe.html										\
				$(OUTPUT_DIR)/05_arrange.html									\
				$(OUTPUT_DIR)/06_slice.html										\
				$(OUTPUT_DIR)/07_boolean_logic.html								\
				$(OUTPUT_DIR)/08_filter.html									\
				$(OUTPUT_DIR)/09_mutate.html									\
				$(OUTPUT_DIR)/10_group_by_and_summarize.html					\
				$(OUTPUT_DIR)/11_other_helpful_dplyr_verbs.html

all : $(OUTPUT_DIR) $(ALL_FILES)

clean :
	$(call cleanup)

$(OUTPUT_DIR) :
	$(call setup_build_directory)

$(XARINGAN) : $(XARINGAN_EXT) | $(OUTPUT_DIR)
	$(call rmarkdown_render,$(XARINGAN_OUTPUT))
