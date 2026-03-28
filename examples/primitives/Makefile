PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: max.qpl

%.qpl: %.traq $(PROJ_DEPS)
	cd ../../ && cabal run traq -- \
		-i examples/primitives/$< \
		-o examples/primitives/$@ \
		-p 0.01
