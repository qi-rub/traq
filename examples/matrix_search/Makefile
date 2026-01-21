PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: matrix_search.qpl depth3_NAND_formula.qpl

matrix_search.qpl: matrix_search.qb $(PROJ_DEPS)
	cd ../../ && cabal run compile -- \
		-i examples/matrix_search/$< \
		-o examples/matrix_search/$@ \
		-p 0.001 \
		--arg N=20 --arg M=10

depth3_NAND_formula.qpl: depth3_NAND_formula.qb $(PROJ_DEPS)
	cd ../../ && cabal run compile -- \
		-i examples/matrix_search/$< \
		-o examples/matrix_search/$@ \
		-p 0.001 \
		--arg N=20 --arg M=10 --arg K=10

