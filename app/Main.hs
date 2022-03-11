module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc

import Options.Applicative
import Data.Semigroup ((<>))
import System.Environment
import Lib
import Parsed
import Typed


data MainArgs = MainArgs
  { mode      :: String
  , outputFile      :: String
  , inputFiles :: String }

parsedArgs :: Parser MainArgs
parsedArgs = MainArgs
    <$> strOption
        ( long "mode"
        <> help "Must be either parse or type" )
    <*> strOption
        ( long "outputFile"
        <> short 'o'
        <> help "Output file path" )
    <*> strOption
        ( long "inputFiles"
        <> short 'i'
        <> help "Space-seperated list of input files"
        <> metavar "TARGETS")

main :: IO ()
main = mainHelp =<< execParser opts
  where
    opts = info (parsedArgs <**> helper)
      ( fullDesc
     <> progDesc "Lib to preprocess haskell source files for GPT to eat up. "
     <> header "Who the fuck uses program headers" )
 
mainHelp :: MainArgs -> IO () 
mainHelp ( MainArgs "parse" outF i ) = do
  let inFs = words i
  mapM_ (processSourceFile outF) inFs

mainHelp ( MainArgs "type" outF i ) = do
  let inFs = words i
  mapM_ (typeAnnotateSource) inFs 

mainHelp _ = return()