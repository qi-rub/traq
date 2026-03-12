PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: matrix_search.qpl depth3_NAND_formula.qpl

matrix_search.qpl: matrix_search.traq $(PROJ_DEPS)
	cd ../../ && cabal run compile -- \
		-i examples/matrix_search/$< \
		-o examples/matrix_search/$@ \
		-p 0.001 \
		--arg N=20 --arg M=10

matrix_search.py: matrix_search.traq $(PROJ_DEPS)
	cd ../../ && cabal run qpltopy -- \
		-i examples/matrix_search/$< \
		-o examples/matrix_search/$@ \
		-p 0.001 \
		--arg N=20 --arg M=10
	python -m py_compile $@
	black $@

matrix_search-qiskit.py: matrix_search.traq $(PROJ_DEPS)
	cd ../../ && cabal run qpltopy -- \
		--backend Qiskit \
		-i examples/matrix_search/$< \
		-o examples/matrix_search/$@ \
		-p 0.1 \
		--arg N=6 --arg M=6
	python -m py_compile $@
	black $@

depth3_NAND_formula.qpl: depth3_NAND_formula.traq $(PROJ_DEPS)
	cd ../../ && cabal run compile -- \
		-i examples/matrix_search/$< \
		-o examples/matrix_search/$@ \
		-p 0.001 \
		--arg N=20 --arg M=10 --arg K=10

