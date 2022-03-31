#!/bin/bash
stack install
HaskellScraper --mode parse -o haskell_standard_libs.txt -i srcs.txt 
HaskellScraper --mode type -o haskell_standard_libs.txt -i srcs.txt 
echo "$(find ../haskell-compilable -type f -name '*.hs')" > compile.txt
echo "$(find ../haskell-base -type f -name '*.hs')" > base.txt
echo "$(find /home/jroper18/.stack/programs/x86_64-linux/ghc-tinfo6-8.10.7/lib/ghc-8.10.7 -type f -name '*.hs')" > stack.txt
