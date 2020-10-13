#!/usr/bin/env bash

set -eu

cabal='cabal'
compilers="ghc-8.2 ghc-8.4 ghc-8.6 ghc-8.8 ghc-8.10"
next_compiler="ghc-9.0.0.20200925"

for ghc in $compilers $next_compiler; do
    echo Testing for ghc-$($ghc --numeric-version)

    build_opt=
    test_opt=
    if [ $ghc == $next_compiler ]; then
        build_opt="--allow-newer"
    fi
    
    $cabal v2-build -w $ghc -v0 --enable-tests -j4 $build_opt \
           matchable matchable-th matchable-examples
    $cabal v2-test  -w $ghc -v0 $build_opt $test_opt \
           matchable-test matchable-th-test
done
