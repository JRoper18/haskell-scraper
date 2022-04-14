#!/bin/bash

OLDPWD=$PWD
rm -r ../typed-results/*
cabal install
find ../haskell-compilable -maxdepth 1 -mindepth 1 -type d \( ! -name . \) -exec bash -c "cd '{}' && cabal clean && cabal build --package-db=/home/jroper18/.cabal/store/ghc-8.10.7/package.db" \;
cd ../typed-results/
cat *.typed.txt > $OLDPWD/haskell_total_typed_libs.txt
cd $OLDPWD