include ../make.inc

DIRS	=	orientation															\
			what_are_comput_data_sci									\
			reproducible_toolbox											\
			data_visualization												\
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
