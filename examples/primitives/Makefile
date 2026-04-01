PROJ_DEPS=../../src ../../tools ../../traq.cabal

all: any.qpl max.qpl

full: all any-qiskit.py 

%.qpl: %.traq $(PROJ_DEPS)
	cd ../../ && cabal run traq -- \
		-i examples/primitives/$< \
		-o examples/primitives/$@ \
		-p 0.01

%-qiskit.py: %.traq
	cd ../../ && cabal run traq -- \
		-i examples/primitives/$< \
		-o examples/primitives/$@ \
		-p 0.01 \
		--target Qiskit
	black $@
