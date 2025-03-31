Proto-qubrabench [![CI](https://github.com/qi-rub/proto-qubrabench/actions/workflows/ci.yml/badge.svg)](https://github.com/qi-rub/proto-qubrabench/actions/workflows/ci.yml)
================

A Haskell implementation of the proto-qubrabench language, along with its cost models and lowerings.

Install
-------

Currently being developed against `GHC 8.6.5`. See the CI for other compatible versions.

1. `cabal build` to build the project.
1. `cabal run` to run the main entry point.
1. `cabal test` to run the tests.

Develop
-------

1. `hpack` to regenerate the `.cabal` configuration (from `package.yaml`).
2. Tests are written using [`hspec`](https://hackage.haskell.org/package/hspec), in `test/`.
