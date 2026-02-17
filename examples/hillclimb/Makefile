PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: max_sat_hillclimb.qpl steep_max_sat.qpl

%.qpl: %.traq $(PROJ_DEPS)
	cd ../../ && cabal run compile -- \
		-i examples/hillclimb/$< \
		-o examples/hillclimb/$@ \
		-p 0.001 \
		--arg n=20 \
		--arg W=1000
