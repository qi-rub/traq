SUBDIRS = cryptanalysis matrix_search tree_generator hillclimb

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

define SUBDIR_RULE
$(1)/%:
	$$(MAKE) -C $(1) $$*
endef

$(foreach dir,$(SUBDIRS),$(eval $(call SUBDIR_RULE,$(dir))))

.PHONY: all $(SUBDIRS)
