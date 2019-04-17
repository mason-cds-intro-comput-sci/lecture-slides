include ../../make.inc

MODULE_NAME	=	$(notdir $(patsubst %/,%,$(CURDIR)))

XARINGAN	=	$(OUTPUT_DIR)/01_gender_discrimination_case_study.html			\
				$(OUTPUT_DIR)/02_a_hypothesis_test_as_a_court_trial.html				\
				$(OUTPUT_DIR)/03_a_manual_simulation_of_the_gender_discrimination_experiment.html				\
				$(OUTPUT_DIR)/04_simulating_case_study_in_r.html				\
				$(OUTPUT_DIR)/05_one-sided_hypothesis_tests.html				\
				$(OUTPUT_DIR)/06_two-sided_hypothesis_tests.html				\
				$(OUTPUT_DIR)/07_variability_of_estimates.html					\
				$(OUTPUT_DIR)/08_simulating_the_confidence_interval.html	\
				$(OUTPUT_DIR)/09_effect_size.html

all : $(OUTPUT_DIR) $(ALL_FILES)

clean :
	$(call cleanup)

$(OUTPUT_DIR) :
	$(call setup_build_directory)

$(XARINGAN) : $(XARINGAN_EXT) | $(OUTPUT_DIR)
	$(call rmarkdown_render,$(XARINGAN_OUTPUT))
