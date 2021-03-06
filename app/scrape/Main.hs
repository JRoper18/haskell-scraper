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
import Hackage
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager)
import System.Process
import GHC.IO.Handle.Text
import Data.Aeson (object, (.=), encode, decode)
import System.Directory
import qualified Data.ByteString.Lazy as BL(singleton, writeFile, intercalate, readFile)
import Data.ByteString.UTF8 as BSU(fromString)     -- from utf8-string
import System.Timeout
import Data.Char(ord)

outPackagesDir = "../haskell-srcs-hackage"

data MainArgs = MainArgs
  { mode      :: String
  , outputFile      :: String
  , inputFiles :: String }

parsedArgs :: Parser MainArgs
parsedArgs = MainArgs
    <$> strOption
        ( long "mode"
        <> help "Must be either parse, type, download, parseDownloaded" )
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

mainHelp ( MainArgs "download" outF i ) = do
  inFsTotal <- readFile i 
  let packages = lines inFsTotal
  manager <- newManager defaultManagerSettings
  mapM_ (\package -> do
        downloadPackageAndSave manager package
    ) packages

mainHelp (MainArgs "parseDownloaded" outF i) = do
  manager <- newManager defaultManagerSettings
  packageDirs <- listDirectory outPackagesDir
  mapM_ (\dir -> do
        parseDownloaded manager (outPackagesDir ++ "/" ++ dir ++ "/")
    ) packageDirs
  putStrLn "Parsed all downloaded! Concatenating to single file..."
  mapM_ (\dir -> do
      examples <- readFile (examplesFilePath dir)
      appendFile outF examples
      appendFile outF "\n"
    )

mainHelp (MainArgs mode _ _) = putStrLn $ "Unknown mode " ++ mode

examplesFilePath :: String -> String 
examplesFilePath packageDir = packageDir ++ "parsed.json"

hsFilesListFilePath :: String -> String 
hsFilesListFilePath outPkgDir = outPkgDir ++ "hsFileList.txt"

sourceContextFilePath :: String -> String 
sourceContextFilePath outPkgDir = outPkgDir ++ "sourceContext.json"

parseDownloaded :: Manager -> String -> IO () 
parseDownloaded manager packageDir = do
  putStrLn ("Parsing package in dir " ++ packageDir)
  let hsFilesListFile = hsFilesListFilePath packageDir
  let sourceContextFile = sourceContextFilePath packageDir 
  -- Parse what you can
  sourceContextFileExists <- (doesFileExist sourceContextFile)
  if sourceContextFileExists then do
    sourceContextRead <- BL.readFile sourceContextFile
    let mbSc' = decode sourceContextRead :: Maybe SourceContext
    case mbSc' of
      Just sc' -> do
        hsFileContents <- readFile hsFilesListFile
        let hsFiles = lines hsFileContents 
        parsedExamplesUnflat <- mapM (\hsF -> getExampleFromSourceContext (outPackagesDir ++ "/") sc'{filePath=hsF}) hsFiles
        let parsedExamples = concat parsedExamplesUnflat
        let examplesFile = examplesFilePath packageDir
        let encoded = map encode parsedExamples
        let bsNl = BL.singleton (fromIntegral ( ord '\n'))
        let wr = BL.intercalate bsNl encoded
        BL.writeFile examplesFile wr
      _ -> return ()
  else putStrLn $ "No source context file found at " ++ sourceContextFile

downloadPackageAndSave :: Manager -> String -> IO () 
downloadPackageAndSave manager package = do
    let tmpTarOutF = "/tmp/tmp.tar.gz"
    sc <- downloadPackageData manager package
    -- Record the dir
    let outPkgDir = outPackagesDir ++ "/" ++ (packageName sc) ++ "-" ++ (packageVersion sc) ++ "/"
    let hsFilesListFile = hsFilesListFilePath outPkgDir
    let sourceContextFile = sourceContextFilePath outPkgDir 
    doesntNeedDownload <- (doesDirectoryExist outPkgDir)
    -- Download it if it's not there. 
    if (not doesntNeedDownload) then do
        putStrLn ("downloading package " ++ package)
        (downloadPackage tmpTarOutF manager sc)
        let tarCmd = "tar -xvf " ++ tmpTarOutF ++ " -C " ++ outPackagesDir
        putStrLn ("done downloading! untarring with command " ++ tarCmd)
        -- Untar the tmp. It'll output a list of files it unzipped, too. 
        (_,Just ho1,_, hp1) <- createProcess (shell tarCmd) {std_out=CreatePipe, std_err=Inherit}
        putStrLn "waiting for untar..."
        sOut <- hGetContents ho1
        -- Wait for timeout for 5 seconds
        timedOut <- timeout 5000000 (waitForProcess hp1)
        if isNothing timedOut then do
          putStrLn "timeout on untar!"
        else do
          putStrLn ("done untarring! saving data")
          -- The HS files from stdout. 
          let hsFiles = filter (\inF -> isSuffixOf ".hs" inF) (lines sOut)
          -- Save the list of HS files extracted
          writeFile hsFilesListFile (intercalate "\n" hsFiles)
          -- Save the context
          let ctxStr = encode sc
          BL.writeFile sourceContextFile ctxStr
          putStrLn ("saved!")
    else putStrLn $ "Package dir already found!"





-- typeProcessSourceFile :: String -> [String] -> String -> IO ()
-- typeProcessSourceFile outF inFs inF = do
--   print inF
--   annotatedMb <- typeAnnotateModuleInSources inFs inF
--   case annotatedMb of
--     Just annotated -> appendFile outF annotated
--     Nothing -> do
--       inContents <- readFile inF
--       appendFile outF (inContents)


