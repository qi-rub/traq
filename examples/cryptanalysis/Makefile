PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: period_finding.qpl even_mansour.qpl 3_round_feistel.qpl grover_meets_simon.qpl

%.qpl: %.traq $(PROJ_DEPS)
	cd ../../ && cabal run compile -- \
		-i examples/cryptanalysis/$< \
		-o examples/cryptanalysis/$@ \
		-p 0.001 \
		--arg n=20 \
		--arg n_plus_1=21
