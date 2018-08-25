include make.inc

DIRS	=	Rmd

.PHONY : all clean env $(DIRS)

all : $(DIRS)

clean :
	$(foreach dir,$(DIRS),cd $(dir) && $(MAKE) clean && cd ../;)

env :
	$(call install_deps)

$(DIRS) :
	$(MAKE) -C $@
