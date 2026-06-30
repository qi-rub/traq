PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: any.qpl max.qpl \
	amplify.qpl camplify.qpl \
	any_det.qpl any_rand.qpl

%.qpl: %.traq $(PROJ_DEPS)
	cd ../../ && cabal run traq -- \
		-i examples/primitives/$< \
		-o examples/primitives/$@ \
		-p 0.01
