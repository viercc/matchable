#!/usr/bin/env bash

set -e

compilers="ghc-8.0 ghc-8.2 ghc-8.4 ghc-8.6 ghc-8.8"

for ghc in $compilers; do
    echo Testing for $($ghc --version)
    
    cabal v2-build -w $ghc -v0 --enable-tests matchable matchable-th matchable-examples
    cabal v2-test -w $ghc -v0 .
    cabal v2-test -w $ghc -v0 matchable-th/

    if [ "$ghc" != "ghc-8.0" ]; then
        cabal v2-build -w $ghc -v0 doctest
        cabal v2-exec -w $ghc -v0 -- doctest -isrc src/**/*.hs
    fi
done
