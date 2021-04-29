#!/usr/bin/env bash

set -ue

cabal="cabal --config-file=$HOME/.cabal/config_nohaddock"
ghcprefix='ghc'
compiler_versions="8.2 8.4 8.6 8.8 8.10 9.0"
next_compiler_version=""

for v in $compiler_versions $next_compiler_version; do
    ghc=${ghcprefix}-${v}
    ghcver=$($ghc --numeric-version)
    echo Testing for ghc-$ghcver
    
    cabal_opt=
    build_opt=
    test_opt="--test-show-details=failures"
    if [ "v$v" == v$next_compiler_version ]; then
        build_opt="$build_opt --allow-newer"
    fi
    
    $cabal v2-build -w $ghc \
           --write-ghc-environment-files=always \
           -v0 --enable-tests -j4 $build_opt \
           matchable matchable-th matchable-examples
    $cabal v2-test -w $ghc -v0 -j4 $test_opt \
           matchable:matchable-test\
           matchable:doctest\
           matchable-th:matchable-th-test
done

