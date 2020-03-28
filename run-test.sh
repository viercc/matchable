#!/usr/bin/env bash

set -e

cabal='cabal'
compilers="ghc-8.2 ghc-8.4 ghc-8.6 ghc-8.8 ghc-8.10"

for ghc in $compilers; do
    echo Testing for ghc-$($ghc --numeric-version)

    $cabal v2-build -w $ghc -v0 -j4 --enable-tests matchable matchable-th matchable-examples
    $cabal v2-test -w $ghc -v0 .
    $cabal v2-test -w $ghc -v0 matchable-th/
done
