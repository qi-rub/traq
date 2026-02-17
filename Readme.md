Traq: Estimating Quantum Cost of Classical Programs [![CI](../../actions/workflows/ci.yml/badge.svg)](../../actions/workflows/ci.yml)
================

A Haskell tool to estimate data-dependent expected quantum costs of high-level classical programs.

Install
-------

Currently being developed against `GHC 9.6.7`. See the CI for other compatible versions.

1. `cabal build` to build the project.
1. `cabal run` to run the main entry point.
1. `cabal test` to run the tests.

Usage
-----

Traq takes high-level classical programs in our prototype language, and produces expected cost estimates.
See [demo.hs](examples/matrix_search/demo.hs) for the code to run the matrix search example.


Contributing
------------

Please see [Contributing.md](Contributing.md).
