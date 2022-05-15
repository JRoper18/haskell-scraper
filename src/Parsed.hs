{-# LANGUAGE DeriveGeneric #-}

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
import System.IO (hPutStrLn, stderr, Handle, withFile, IOMode(..), hGetContents, openFile, hSetEncoding)
import GHC.Unicode
import GhcPlugins
import GHC.Generics
import GHC.Exception (SomeException)
import GHC.IO.Encoding
import Hackage (SourceContext, filePath)
import Data.Aeson (object, (.=), encode, decode, ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)


data ParsedExample = ParsedExample {
  context :: SourceContext,
  content :: String,
  binds :: [String],
  name :: String,
  signature :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance ToJSON ParsedExample where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ParsedExample

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

parsedExampleFromBinds :: SourceContext -> [String] -> String -> Maybe String -> ParsedExample
parsedExampleFromBinds sc binds name sig = do
  ParsedExample {
    context = sc,
    content = intercalate "\n" binds,
    binds = binds,
    name = name,
    signature = sig
  }

getExampleFromSourceContext :: FilePath -> SourceContext -> IO ( [ParsedExample] ) 
getExampleFromSourceContext prefixPath context = do
  let targetFile = prefixPath ++ (filePath context)
  parsedSourceEi <- parseSource targetFile
  case parsedSourceEi of
    Right parsedSource -> do
        dflags <- runGhc (Just libdir) $ do
          getSessionDynFlags
        let docMaker = showSDoc dflags
        let namedDecls = getNamedDecls docMaker (hsmodDecls (unpackLocatedData parsedSource))
        let decls = map fst namedDecls
        let groupedDecls = filter (not . null) (groupBy ( declNamesEqual docMaker) decls)
        let sigDecls = map ((\decl -> case (unpackLocatedData decl) of 
                                 SigD _ _ -> Just decl
                                 _ -> Nothing     
                            ) . head) groupedDecls
        let sigStrs = map (\declMb -> case declMb of
                                       Just decl -> Just ((showDecl docMaker) decl)
                                       Nothing -> Nothing) sigDecls
        let groupedDeclStrs = (map ( map ( showDecl docMaker ) ) groupedDecls )
        let declNames = catMaybes (map (getDeclName docMaker . head) (groupedDecls))
        if (length declNames) /= (length groupedDeclStrs) then do
          putStrLn "PANIC ON NAME DETECTION" 
          print declNames
          print groupedDeclStrs 
          return [] 
        else do 
          return $ map (uncurry3 (parsedExampleFromBinds context)) (zip3 groupedDeclStrs declNames sigStrs) 
    Left x -> do
      return []
  

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

treeDeclStrsFromParsedSource = declStrsFromParsedSource showData

plainDeclStrsFromParsedSource :: (SDoc -> String) -> ParsedSource -> [[String]]
plainDeclStrsFromParsedSource docMaker = declStrsFromParsedSource (showDecl docMaker) docMaker

declStrsFromParsedSource :: (GHC.LHsDecl ( GHC.GhcPs ) -> String) -> (SDoc -> String) -> ParsedSource -> [[String]]
declStrsFromParsedSource serializer docMaker psource = do
  let decls = hsmodDecls ( unpackLocatedData (psource) )
  let groupedDecls = groupBy ( declNamesEqual docMaker) decls
  let groupedDeclStrs = map ( map ( serializer ) ) groupedDecls
  groupedDeclStrs
  
unconcatedDeclStrs :: String -> IO (Either ErrUtils.ErrorMessages [[String]]) 
unconcatedDeclStrs targetFile = do
  parsedSource <- parseSource targetFile
  dflags <- runGhc (Just libdir) $ do
    getSessionDynFlags
  let docMaker = showSDoc dflags
  case parsedSource of
    Right res -> do
      return ( Right ( plainDeclStrsFromParsedSource docMaker res ) )
    Left x -> do
      return (Left x)


concatDeclStrsSimple :: [[String]] -> String 
concatDeclStrsSimple strs = do
  intercalate "\n<|splitter|>\n" ( map ( intercalate "\n" ) strs )
  
declStrs :: String -> IO (Either ErrUtils.ErrorMessages String)
declStrs targetFile = do
  unconcated <- unconcatedDeclStrs targetFile
  case unconcated of
    Right res -> do
      return ( Right ( concatDeclStrsSimple res ) )
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
    inputHandle <- openFile targetFile ReadMode 
    hSetEncoding inputHandle latin1
    fileContents <- hGetContents inputHandle
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
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        ctx <- getContext
        -- let declStr = "module Decls where\n" ++ intercalate "\n" ( map ( docMaker . ppr) decls )
        -- liftIO (Prelude.writeFile "./Decls.hs" declStr)
        importDecls <- mapM parseImportDecl ["import Prelude"]
        interactiveImports <- mapM (\decl -> return $ IIDecl decl) importDecls
        setContext interactiveImports
        let declStrs = map ( docMaker . ppr) decls
        let declStr = intercalate "\n" declStrs
        liftIO ( putStrLn declStr )
        names <- runDecls declStr
        liftIO (putStrLn . docMaker . ppr $ names )
        -- mapM_ (\stmt -> execStmt stmt execOptions ) ["f :: Int -> Int", "f 0 = 1", "f n = n"]
        let fnameMb = getDeclName docMaker $ head decls
        case fnameMb of
          Just fname -> do
            -- let stmt = fname ++ " " ++ subArg
            let stmt = "x"
            liftIO ( putStrLn stmt )
            execRes <- execStmt stmt execOptions
            case execRes of
              ExecComplete execResReal alloc -> do
                case execResReal of
                  Right nameArr -> return $ docMaker $ ppr nameArr
                  Left  exn  -> return $ docMaker (text "*** Exception:" <+>
                                          text (show (exn :: SomeException)))
              ExecBreak _ _ -> return "ExecBreak"
          Nothing -> return "Could not get function name"

-- evalToStr' :: (SDoc -> String) -> [LHsDecl GhcPs] -> String -> GhcMonad (String)
-- evalToStr' docMaker decls subArgs = do
  