#!/usr/bin/env bash

set -ue

compiler_versions="9.0 9.2 9.4 9.6"

for v in $compiler_versions; do
    ghc_exact_ver=$(ghcup run --ghc $v -- ghc --numeric-version)
    cabal="ghcup run --ghc $ghc_exact_ver --cabal latest -- cabal --config-file=$HOME/.cabal/config_nohaddock"
    echo == ghc-${ghc_exact_ver} ==
    
    echo Build the library
    build_opt="-v0 -j4"
    test_opt="--test-show-details=failures"
    
    $cabal v2-build \
           --write-ghc-environment-files=always \
           --enable-tests $build_opt \
           matchable matchable-th
    
    echo Build examples
    $cabal v2-build \
           --enable-tests $build_opt \
           matchable-examples
    
    echo Run tests
    $cabal v2-test $build_opt $test_opt \
           matchable:matchable-test\
           matchable-th:matchable-th-test
    
    echo Install doctest for the current compiler
    doctest_dir="./doctest-bin/ghc-${ghc_exact_ver}"
    mkdir -p "$doctest_dir"
    $cabal v2-install $build_opt \
      "--installdir=$doctest_dir" --overwrite-policy=always \
      doctest --allow-newer

    echo Run doctest
    $cabal v2-repl "--with-ghc=$doctest_dir/doctest"
done
