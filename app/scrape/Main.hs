module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc

import Options.Applicative
import Data.Semigroup ((<>))
import System.Environment
import Parsed
import Data.Maybe
import Typed
import Data.List

data MainArgs = MainArgs
  { mode      :: String
  , outputFile      :: String
  , inputFiles :: String }

parsedArgs :: Parser MainArgs
parsedArgs = MainArgs
    <$> strOption
        ( long "mode"
        <> help "Must be either parse or type." )
    <*> strOption
        ( long "outputFile"
        <> short 'o'
        <> help "Output file path" )
    <*> strOption
        ( long "inputFilesList"
        <> short 'i'
        <> help "Newline-seperated file list of input files."
        <> metavar "TARGETS")

main :: IO ()
main = mainHelp =<< execParser opts
  where
    opts = info (parsedArgs <**> helper)
      ( fullDesc
     <> progDesc "A library to preprocess haskell source files for GPT to eat up. "
     <> header "Who the fuck uses program headers" )
 
mainHelp :: MainArgs -> IO () 
mainHelp ( MainArgs "parse" outF i ) = do
  inFsTotal <- readFile i 
  let inFs = lines inFsTotal
  mapM_ (processSourceFile outF) inFs

mainHelp ( MainArgs "type" outF i ) = do
  inFsTotal <- readFile i 
  let inFs = lines inFsTotal
  let hsFiles = filter (\inF -> isSuffixOf ".hs" inF) inFs
  mapM_ (\inF -> do
      res <- typeAnnotatePackage inF
      mapM_ (appendFile outF) (catMaybes res)
    ) inFs 

mainHelp _ = return()

-- typeProcessSourceFile :: String -> [String] -> String -> IO ()
-- typeProcessSourceFile outF inFs inF = do
--   print inF
--   annotatedMb <- typeAnnotateModuleInSources inFs inF
--   case annotatedMb of
--     Just annotated -> appendFile outF annotated
--     Nothing -> do
--       inContents <- readFile inF
--       appendFile outF (inContents)


