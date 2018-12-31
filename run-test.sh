#!/usr/bin/env bash

set -e

compilers="ghc-8.0.2 ghc-8.2.2 ghc-8.4.3 ghc-8.6.3"
doctest_compilers="ghc-8.2.2 ghc-8.4.3 ghc-8.6.3"

for ghc in $compilers; do
    cabal v2-build -w $ghc --enable-tests matchable matchable-examples
    cabal v2-test -w $ghc
done

for ghc in $doctest_compilers; do
    cabal v2-build -w $ghc doctest
    cabal v2-exec -w $ghc -- doctest -isrc src/**/*.hs
done
