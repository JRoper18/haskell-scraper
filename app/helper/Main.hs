module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc

import Options.Applicative
import Data.Semigroup ((<>))
import System.Environment
import Lib
import Parsed
import Data.Maybe
import Typed
import Data.List
import Parsed
import LibUtil
import GHC (hsmodDecls, LHsDecl, GhcPs)
import LibUtil
import LibUtil (makeDocMaker)
import qualified System.IO.Strict as S

data Input
  = FileInput FilePath
  | StdInput

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )

input :: Parser Input
input = fileInput <|> stdInput

inputToStr :: Input -> IO ( String )
inputToStr StdInput = S.getContents
inputToStr (FileInput fin) = readFile fin

data MainArgs = MainArgs
  { mode      :: String
  , mainInput :: Input}

parsedArgs :: Parser MainArgs
parsedArgs = MainArgs
    <$> strOption
        ( long "mode"
        <> help "Mode to get data on. Must be ast, pretty, or eval." )
    <*> input
    

main :: IO ()
main = mainHelp =<< execParser opts
  where
    opts = info ((parsedArgs) <**> helper)
      ( fullDesc
     <> progDesc "A library to allow for quick evaluation of GPT-generated haskell ASTs"
     <> header "Who the fuck uses program headers" )

mainHelp :: MainArgs -> IO ()
mainHelp ( MainArgs "ast" mainIn ) = do
  inputStr <- inputToStr mainIn
  parsedModEither <- parseStrSource inputStr
  case parsedModEither of
    Right parsedMod -> do
      let decls = hsmodDecls ( unpackLocatedData parsedMod )
      putStrLn $ showData $ head decls
    Left err -> do
      printErrMessages err

mainHelp ( MainArgs "pretty" mainIn ) = do
  inputStr <- inputToStr mainIn
  let declMb = readData inputStr :: Maybe (LHsDecl GhcPs)
  case declMb of
    Just decl -> do
      docMaker <- makeDocMaker
      (putStrLn . showDecl docMaker) decl
    Nothing -> do
      putStrLn "Bad decl input"

-- mainHelp ( MainArgs "eval" inputStr ) = do
--     parsedModEither <- parseStrSource inputStr
  -- case parsedModEither of
  --   Right parsedMod -> do
  --     let decls = hsmodDecls ( unpackLocatedData parsedMod )
  --     mapM_ (putStrLn . showDecl docMaker) decls
  --   Left err -> do
  --     printErrMessages err

mainHelp _ = return()

