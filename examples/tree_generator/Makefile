PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: loop_example.qpl tree_generator_01_knapsack.qpl

%.qpl: %.traq $(PROJ_DEPS)
	cd ../../ && cabal run compile -- \
		-i examples/tree_generator/$< \
		-o examples/tree_generator/$@ \
		-p 0.001 \
		--arg W=1000 \
		--arg P=1000 \
		--arg N=10 \
		--arg K=3
