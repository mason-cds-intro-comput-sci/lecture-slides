include ../make.inc

DIRS	=	orientation															\
			what_are_comput_data_sci											\
			reproducible_toolbox												\
			data_visualization													\
			data_transformation													\
			data_reshaping

.PHONY : all clean $(DIRS)

all : $(DIRS)

clean :
	$(foreach dir,$(DIRS),cd $(dir) && $(MAKE) clean && cd ../;)

$(DIRS) :
	$(MAKE) -C $@
