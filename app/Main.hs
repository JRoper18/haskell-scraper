-- module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc


{-# LANGUAGE CPP #-}
import GHC
import Outputable
import GHC.Paths ( libdir )
import DynFlags
import Data.Dynamic
import System.Posix.Internals (newFilePath)
import ErrUtils
import Data.Either
import Data.List
import Bag
import System.IO (hPutStrLn, stderr)
import System.Environment

unpackLocatedData :: GHC.Located( p ) -> p
unpackLocatedData (L l m) = m

unpackLocatedLocation :: GHC.Located( p ) -> GHC.SrcSpan
unpackLocatedLocation (L l m) = l

showDecl :: DynFlags -> GHC.LHsDecl ( GHC.GhcPs ) -> String
showDecl dflags decl = do
    let docMaker = showSDoc ( dflags )
    let strList = docMaker ( ppr ( decl ) )
    strList

keepDecl :: GHC.LHsDecl ( GHC.GhcPs ) -> Bool
keepDecl decl = case ( unpackLocatedData decl ) of
  SigD _ _ -> True
  ValD _ _ -> True
  _ -> False

printErrMessages :: ErrorMessages -> IO ()
printErrMessages msgs = do
  let bagL = bagToList msgs
  let strs = map show bagL
  mapM_ ( hPutStrLn stderr ) strs 

processSourceFile :: String -> String -> IO ()
processSourceFile outF inF = do
  res <- declStrs inF
  case res of 
    Right x -> do 
      appendFile outF x
      appendFile outF "\n\n";
    Left x -> do 
      printErrMessages x
main :: IO ()
main = do
  args <- getArgs
  let outF = head args
  let inFs = tail args
  mapM_ (processSourceFile outF) inFs
 
declStrs :: String -> IO (Either ErrUtils.ErrorMessages String)
declStrs targetFile = do
  parsedSource <- parseSource targetFile
  dflags <- runGhc (Just libdir) $ do
        getSessionDynFlags
  let docMaker = showSDoc dflags
  case parsedSource of
    Right res -> do
      -- print (dynTypeRep (toDyn res))
      -- print (dynTypeRep (toDyn dflags))
      -- print ppr ( moduleFromSource res)
      let decls = hsmodDecls ( unpackLocatedData (res) )
      let goodDecls = filter keepDecl decls
      let declStrs = map ( showDecl dflags ) goodDecls
      let finalStr = intercalate "\n" declStrs
      return ( Right finalStr )
    Left x -> do
      return (Left x)

parseSource :: String -> IO (Either ErrUtils.ErrorMessages ParsedSource)
parseSource targetFile =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      fileContents <- readFile targetFile
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        -- let dflags' = foldl xopt_set dflags
        --                     [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        -- setSessionDynFlags dflags
        -- target <- guessTarget targetFile Nothing
        -- setTargets [target]
        -- load LoadAllTargets
        let parseFullRes = parser fileContents dflags targetFile
        let parseRes = snd parseFullRes
        return parseRes
        -- let ret = either ( Left ) ( $ Right parsedSource ) 
        -- return ret
        -- modSum <- getModSummary $ mkModuleName targetFile
        -- p <- parseModule modSum
        -- t <- typecheckModule p
        -- d <- desugarModule t
        -- l <- loadModule d
        -- n <- getNamesInScope
        -- c <- return $ coreModule d

        -- g <- getModuleGraph
        -- return $ parsedSource d