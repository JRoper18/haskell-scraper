module Parsed where

import GHC
import Outputable
import GHC.Paths ( libdir )
import DynFlags
import Data.Dynamic
import System.Posix.Internals (newFilePath)
import ErrUtils
import Data.Either
import Data.List
import Data.Maybe
import Data.Data
import Data.ByteString.Lazy.Char8 (unpack, writeFile)
import Data.Char (isSpace)
import LibUtil
import CoreUtils
import Bag
import Desugar
import TcSMonad
import TcRnTypes
import System.IO (hPutStrLn, stderr, Handle, withFile, IOMode(..), hGetContents)
import GHC.Unicode
import GhcPlugins
import Data.Aeson
import GHC.Generics
import GHC.Exception (SomeException)

showOutputable :: Outputable a => (SDoc -> String) -> a -> String
showOutputable docMaker decl = docMaker ( ppr ( decl ) )

showDecl :: (SDoc -> String) -> GHC.LHsDecl ( GHC.GhcPs ) -> String
showDecl docMaker decl = do
    let strList = docMaker ( ppr ( decl ) )
    -- let strList = unpack $ encode decl
    -- let strList = showData decl
    strList

keepDecl :: GHC.LHsDecl ( GHC.GhcPs ) -> Bool
keepDecl decl = isJust ( getDeclIds (decl) )

processSourceFile :: String -> String -> IO ()
processSourceFile outF inF = do
  res <- declStrs inF
  case res of
    Right x -> do
      appendFile outF x
      -- If the string isn't blank add a splitter. 
      if not ( all isSpace x ) then appendFile outF "\n<|splitter|>\n" else return ()
    Left x -> return ()
      -- printErrMessages x

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

getDeclNamePair :: (SDoc -> String) -> LHsDecl GhcPs -> Maybe (LHsDecl GhcPs, String)
getDeclNamePair docMaker decl = case ( getDeclName docMaker decl ) of
    Just name -> Just (decl, name)
    Nothing -> Nothing

getNamedDecls :: (SDoc -> String) -> [LHsDecl GhcPs] -> [(LHsDecl GhcPs, String)]
getNamedDecls docMaker decls = do
    let declMaybeNames = map (getDeclNamePair docMaker) decls
    catMaybes declMaybeNames

declNamesEqual :: (SDoc -> String) -> LHsDecl GhcPs -> LHsDecl GhcPs -> Bool
declNamesEqual docMaker decl1 decl2 = getDeclName docMaker decl1 == getDeclName docMaker decl2

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
      let groupedDecls = groupBy ( declNamesEqual docMaker) decls
      let groupedDeclStrs = map ( map ( showData ) ) groupedDecls
      let finalStr = intercalate "\n<|splitter|>\n" ( map ( intercalate "\n" ) groupedDeclStrs )
      return ( Right finalStr )
    Left x -> do
      return (Left x)

parseStrSource :: String -> IO (Either ErrUtils.ErrorMessages ParsedSource)
parseStrSource fileContents = do
  let tmpFile = "/tmp/toBParsed.hs"
  Prelude.writeFile tmpFile fileContents
  ret <- parseSource tmpFile
  -- removeFile tmpFile
  return ret


parseSource :: FilePath -> IO (Either ErrUtils.ErrorMessages ParsedSource)
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
      let filteredTxt = filter (\s -> isAscii s && (isSpace s || not ( isControl s))) fileContents
      let parseFullRes = parser filteredTxt dflags targetFile
      let parseRes = snd parseFullRes
      return parseRes


evalToStr :: (SDoc -> String) -> [LHsDecl GhcPs] -> String -> IO (String)
evalToStr docMaker decls subArgs = do
    runGhc (Just libdir) $ do
        setSessionDynFlags =<< getSessionDynFlags
        ( evalToStr' docMaker decls subArgs )

evalToStr' :: (SDoc -> String) -> [LHsDecl GhcPs] -> String -> Ghc (String)
evalToStr' docMaker decls subArgs = do
  let declStr = intercalate "\n" ( map ( docMaker . ppr) decls )
  liftIO ( putStrLn declStr )
  names <- runDecls declStr
  liftIO ( mapM_ (putStrLn . docMaker . ppr) names )
  let fnameMb = getDeclName docMaker $ head decls
  case fnameMb of
    Just fname -> do
      liftIO ( putStrLn fname)
      let stmt = fname ++ " " ++ subArgs
      execRes <- execStmt stmt execOptions
      case execRes of
        ExecComplete execResReal alloc -> do
          case execResReal of
            Right nameArr -> return $ docMaker $ ppr nameArr
            Left  exn  -> return $ docMaker (text "*** Exception:" <+>
                                    text (show (exn :: SomeException)))
        ExecBreak _ _ -> return "ExecBreak"
    Nothing -> return "Could not get function name"