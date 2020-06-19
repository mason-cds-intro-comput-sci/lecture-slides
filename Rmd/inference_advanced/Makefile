include ../../make.inc

MODULE_NAME	=	$(notdir $(patsubst %/,%,$(CURDIR)))

XARINGAN	=	$(OUTPUT_DIR)/01_inference_review.html     				\
            $(OUTPUT_DIR)/02_one_vs_two_sided_tests.html      \
            $(OUTPUT_DIR)/03_multiple_comparisons.html        \
            $(OUTPUT_DIR)/04_point_comparisons.html           \
            $(OUTPUT_DIR)/05_effect_sizes.html                \
            $(OUTPUT_DIR)/06_confidence_intervals.html        \
            $(OUTPUT_DIR)/07_permutation_vs_bootstrap.html

all : $(OUTPUT_DIR) $(ALL_FILES)

clean :
	$(call cleanup)

$(OUTPUT_DIR) :
	$(call setup_build_directory)

$(XARINGAN) : $(XARINGAN_EXT) | $(OUTPUT_DIR)
	$(call rmarkdown_render,$(XARINGAN_OUTPUT))
