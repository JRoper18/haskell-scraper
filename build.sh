#!/bin/bash
stack run -- --mode parse -o haskell_standard_libs.txt -i "$(find ../haskell-srcs -type f -name '*.hs')"