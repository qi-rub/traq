Traq: Estimating Quantum Cost of Classical Programs [![CI](../../actions/workflows/ci.yml/badge.svg)](../../actions/workflows/ci.yml)
================

A Haskell tool to estimate data-dependent expected quantum costs of high-level classical programs.

Install
-------

Currently being developed against `GHC 9.6.7`. See the CI for other compatible versions.

1. `cabal build` to build the project.
1. `cabal run traq` to run the main entry point.
1. `cabal test` to run the tests.

Usage
-----

Traq takes high-level classical programs in our prototype language, and produces expected cost estimates.
See [demo.hs](examples/matrix_search/demo.hs) for the code to run the matrix search example.


Contributing
------------

Please see [Contributing.md](Contributing.md).

Paper
-----

The tool is based on the following paper:

```bibtex
@misc{peduri2026traqestimatingquantumcost,
      title={Traq: Estimating the Quantum Cost of Classical Programs}, 
      author={Anurudh Peduri and Jam Kabeer Ali Khan and Gilles Barthe and Michael Walter},
      year={2026},
      eprint={2509.01508},
      archivePrefix={arXiv},
      primaryClass={quant-ph},
      url={https://arxiv.org/abs/2509.01508}, 
}
```
