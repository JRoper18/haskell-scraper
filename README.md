# HaskellScraper


This project is a bunch of varied tools used to scrape and process Haskell source code into a large, standardized corpus. The corpus itself can be found at corpus.tar.gz, and is ~525Mb uncompressed. The packages scraped for the corpus can be found in packages-50more.txt, which contains a list of all Hackage packages with 50 or more monthly downloads (as of May 2022). 

There are three main tools in this repo: 
* The Helper app
* The Scraper app
* The IntermediatePlugin for GHC

## Helper
The Helper application is made to help turn serialize haskell source ASTs into an AST string format and back into source code. There's different formats for Typed trees as opposed to simple parse trees. Check command line arguments to see it's function. 

## Scraper
The Scraper is mostly used for scraping haskell source code and coalating it into a corpus. It can parse or (attempt to) typecheck a list of source files. It's best functions are the "download" and "parseDownloaded" functions, which will download from a list of hackage packages, and then respectively get the function declarations and signatures from them, in order to combine them into a single corpus. 

## IntermediatePlugin
This is a plugin that, when interloaded into the GHC during compilation, is able to generate parsed and/or typed AST representations of the source during compilation. This is much more likely to succeed, given that it can integrate better with Cabal.

