PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: matrix_search.qpl

matrix_search.qpl: matrix_search.qb $(PROJ_DEPS)
	cd ../../ && cabal run compile -- \
		-i examples/matrix_search/matrix_search.qb \
		-o examples/matrix_search/matrix_search.qpl \
		-p 0.001 \
		--arg N=20 --arg M=10
