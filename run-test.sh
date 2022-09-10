#!/usr/bin/env bash

set -ue

compiler_versions="8.8.4 8.10.7 9.0.2 9.2.4"
next_compiler_version="9.4.2"

for v in $compiler_versions $next_compiler_version; do
    cabal="ghcup run --ghc $v --cabal latest -- cabal --config-file=$HOME/.cabal/config_nohaddock"
    echo == ghc-$v ==
    
    echo Build the library
    build_opt="-v0 -j4"
    # 
    #     build_opt="$build_opt --project-file=cabal.project.ghc-9.2.0-rc1 --allow-newer"
    # fi
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
    
    if [ ! "v$v" == v$next_compiler_version ]; then
       echo Install doctest for the current compiler
       doctest_dir="./doctest-bin/ghc-${v}"
       mkdir -p "$doctest_dir"
       $cabal v2-install $build_opt \
         "--installdir=$doctest_dir" --overwrite-policy=always \
         doctest --allow-newer

       echo Run doctest
       $cabal v2-repl "--with-ghc=$doctest_dir/doctest"
    fi
done
