module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc

import Options.Applicative
import Data.Semigroup ((<>))
import System.Environment
import Lib
import Data.Maybe
import Typed
import Data.List
import GHC (hsmodDecls, LHsDecl, GhcPs, runGhc, runParsedDecls, execStmt, execOptions, ParsedSource, GhcMonad, Ghc)
import LibUtil
import qualified System.IO.Strict as S
import Parsed
import Data.Char (isSpace)
import GhcPlugins (ppr)
import System.IO

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
  , mainInput :: Input
  , subArgs :: Maybe String}

parsedArgs :: Parser MainArgs
parsedArgs = MainArgs
    <$> strOption
        ( long "mode"
        <> short 'm'
        <> help "Mode to get data on. Must be ast, pretty, or eval (deprecated, use runhaskell)." )
    <*> input
    <*> optional (strOption
        ( long "subArgs"
        <> short 's'
        <> help "When using eval mode, these are a space-seperated list of args to pass to the function. " ) )

main :: IO ()
main = mainHelp =<< execParser opts
  where
    opts = info ((parsedArgs) <**> helper)
      ( fullDesc
     <> progDesc "A library to allow for quick evaluation of GPT-generated haskell ASTs"
     <> header "Who the fuck uses program headers" )

mainHelp :: MainArgs -> IO ()
mainHelp ( MainArgs "ast" mainIn subArgs ) = do
  inputStr <- inputToStr mainIn
  parsedModEither <- parseStrSource inputStr
  case parsedModEither of
    Right parsedMod -> do
      let decls = hsmodDecls ( unpackLocatedData parsedMod )
      putStrLn $ intercalate "\n" (map showData decls) 
    Left err -> do
      printErrMessages err

mainHelp ( MainArgs "pretty" mainIn subArgs ) = do
  inputStr <- inputToStr mainIn
  let declStrs = filter (not . (all isSpace)) (lines inputStr)
  mapM_ (\declS -> if isNothing (readData (declS) :: Maybe (LHsDecl GhcPs)) then hPutStrLn stderr ("Bad decl:\n" ++ declS ++ "\n") else return()) declStrs
  let decls = map readData (lines inputStr) :: [Maybe (LHsDecl GhcPs)]
  docMaker <- makeDocMaker
  mapM_ (putStrLn . docMaker . ppr) (catMaybes decls)

mainHelp ( MainArgs "eval" mainIn (Just subArgs) ) = do
  docMaker <- makeDocMaker
  inputStr <- inputToStr mainIn
  let declStrs = filter (not . (all isSpace)) (lines inputStr)
  mapM_ (\declS -> if isNothing (readData (declS) :: Maybe (LHsDecl GhcPs)) then putStrLn ("Bad decl" ++ declS) else return()) declStrs
  let decls = map readData (lines inputStr) :: [Maybe (LHsDecl GhcPs)]
  strRes <- evalToStr docMaker (catMaybes decls) subArgs
  putStrLn strRes

mainHelp _ = return()

