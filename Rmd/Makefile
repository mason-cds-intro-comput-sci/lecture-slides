include ../make.inc

DIRS	=	orientation															\
			01_what_are_comput_data_sci									\
			02_reproducible_toolbox											\
			03_data_visualization												\
			data_transformation												\
			git_vcs 																	\
			inference_advanced           							\
			prediction                  							\
			data_reshaping														\
			data_distributions												\
			inference_and_simulation                  \
			modeling                                  \
			final_project

.PHONY : all clean $(DIRS)

all : $(DIRS)

clean :
	$(foreach dir,$(DIRS),cd $(dir) && $(MAKE) clean && cd ../;)

$(DIRS) :
	$(MAKE) -C $@
