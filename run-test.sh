#!/usr/bin/env bash

set -ue

cabal="cabal --config-file=$HOME/.cabal/config_nohaddock"
ghcprefix='ghc'
compiler_versions="8.8 8.10 9.0 9.2 9.4"
next_compiler_version=""

for v in $compiler_versions $next_compiler_version; do
    ghc=${ghcprefix}-${v}
    ghcver=$($ghc --numeric-version)
    echo Testing for ghc-$ghcver
    
    build_opt="-v0 -j4"
    # if [ "v$v" == v$next_compiler_version ]; then
    #     build_opt="$build_opt --project-file=cabal.project.ghc-9.2.0-rc1 --allow-newer"
    # fi
    test_opt="--test-show-details=failures"
    
    $cabal v2-build -w $ghc \
           --write-ghc-environment-files=always \
           --enable-tests $build_opt \
           matchable matchable-th matchable-examples
    $cabal v2-test -w $ghc $build_opt $test_opt \
           matchable:matchable-test\
           matchable-th:matchable-th-test
    
    ## temporarily disabled
    #$cabal v2-test -w $ghc -v0 -j4 $build_opt $test_opt matchable:doctest
done

