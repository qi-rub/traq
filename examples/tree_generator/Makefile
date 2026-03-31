PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: tree_generator_01_knapsack.qpl

%.qpl: %.traq $(PROJ_DEPS)
	cd ../../ && cabal run traq -- \
		-i examples/tree_generator/$< \
		-o examples/tree_generator/$@ \
		-p 0.001 \
		--arg W=1000 \
		--arg P=1000 \
		--arg N=4 \
		--arg K=3 \
		--argf p=0.2
