#!/bin/bash
stack install
HaskellScraper --mode parse -o haskell_standard_libs.txt -i srcs.txt 
