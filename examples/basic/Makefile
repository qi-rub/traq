PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: for_loop.qpl

%.qpl: %.traq $(PROJ_DEPS)
	cd ../../ && cabal run traq -- \
		-i examples/basic/$< \
		-o examples/basic/$@ \
		-p 0.001 \
		--arg W=1000 \
		--arg P=1000 \
		--arg N=10 \
		--arg K=3
