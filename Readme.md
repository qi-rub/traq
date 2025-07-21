Traq: Estimating Quantum Cost of Classical Programs [![CI](/actions/workflows/ci.yml/badge.svg)](/actions/workflows/ci.yml)
================

A Haskell tool to estimate data-dependent expected quantum costs of high-level classical programs.

Install
-------

Currently being developed against `GHC 9.6.7`. See the CI for other compatible versions.

1. `cabal build` to build the project.
1. `cabal run` to run the main entry point.
1. `cabal test` to run the tests.

Develop
-------

1. `hpack` to regenerate the `.cabal` configuration (from `package.yaml`).
2. Tests are written using [`hspec`](https://hackage.haskell.org/package/hspec), in `test/`.
3. Formatting: [`formolu`](https://fourmolu.github.io/).
