module Lib where


import GHC
import Outputable
import GHC.Paths ( libdir )
import DynFlags
import Data.Dynamic
import System.Posix.Internals (newFilePath)
import ErrUtils
import Data.Either
import Data.List
import Data.Char (isSpace)
import Bag
import System.IO (hPutStrLn, stderr)


unpackLocatedData :: GHC.Located( p ) -> p
unpackLocatedData (L l m) = m

unpackLocatedLocation :: GHC.Located( p ) -> GHC.SrcSpan
unpackLocatedLocation (L l m) = l

showDecl :: (SDoc -> String) -> GHC.LHsDecl ( GHC.GhcPs ) -> String
showDecl docMaker decl = do
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
      -- If the string isn't blank add a splitter. 
      if not ( all isSpace x ) then appendFile outF "\n<|splitter|>\n" else putStrLn "file no functions"
    Left x -> do 
      printErrMessages x

getDeclIds :: LHsDecl GhcPs -> Maybe [LIdP GhcPs]
getDeclIds decl = case ( unpackLocatedData decl ) of 
    SigD _ sig -> case sig of 
        TypeSig _ ids _ -> Just ids
        PatSynSig _ ids _ -> Just ids
        ClassOpSig _ _ ids _ -> Just ids
        _ -> Nothing
    ValD _ bind -> case bind of 
        FunBind _ id _ _ _ -> Just [id]
        _ -> Nothing
    _ -> Nothing


getDeclName :: (SDoc -> String) -> LHsDecl GhcPs -> Maybe String 
getDeclName docMaker decl = do
    let maybeIds = getDeclIds decl
    case maybeIds of
        Just ids -> Just ( docMaker ( ppr ( head ids ) ) )
        Nothing -> Nothing


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
      let declStrs = map ( showDecl docMaker ) goodDecls
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
        