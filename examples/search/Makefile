PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: clustering.qpl triangle_finding.qpl

clustering.qpl: clustering.traq $(PROJ_DEPS)
	cd ../../ && cabal run traq -- \
		-i examples/search/$< \
		-o examples/search/$@ \
		-p 0.001 \
		--arg N=10 \
		--arg M=100

triangle_finding.qpl: triangle_finding.traq $(PROJ_DEPS)
	cd ../../ && cabal run traq -- \
		-i examples/search/$< \
		-o examples/search/$@ \
		-p 0.001 \
		--arg N=10
