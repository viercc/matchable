#!/usr/bin/env bash

set -e

compilers="ghc-8.0 ghc-8.2 ghc-8.4 ghc-8.6 ghc-8.8"
doctest_compilers="ghc-8.2 ghc-8.4 ghc-8.6 ghc-8.8"

for ghc in $compilers; do
    cabal v2-build -w $ghc --enable-tests matchable matchable-th matchable-examples
    cabal v2-test -w $ghc .
    cabal v2-test -w $ghc matchable-th/
done

for ghc in $doctest_compilers; do
    cabal v2-build -w $ghc doctest
    cabal v2-exec -w $ghc -- doctest -isrc src/**/*.hs
done
