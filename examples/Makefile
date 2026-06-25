SUBDIRS = basic primitives cryptanalysis matrix_search tree_generator hillclimb search

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

define SUBDIR_RULE
$(1)/%:
	$$(MAKE) -C $(1) $$*
endef

$(foreach dir,$(SUBDIRS),$(eval $(call SUBDIR_RULE,$(dir))))

# Generate the augmented Table 2 statistics (QPL size, error/cost bounds) for all
# case studies: prints a Markdown table and writes case_study_stats.csv.
stats:
	cd .. && cabal run -v0 casestudystats

.PHONY: all stats $(SUBDIRS)
