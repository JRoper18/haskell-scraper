{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Typed where

import GHC
import Outputable
import GHC.Paths ( libdir )
import DynFlags ( defaultFatalMessager, defaultFlushOut )
import Data.Dynamic
import System.Posix.Internals (newFilePath)
import ErrUtils
import Data.Either
import Data.List
import Data.Maybe
import Data.Data
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (isSpace)
import LibUtil
import CoreUtils
import Bag
import Desugar
import TcRnTypes
import System.IO (hPutStrLn, stderr)
import GHC.Unicode
import GhcPlugins
import Data.Aeson
import GHC.Generics
import Text.Regex.Posix
import EnumSet
import Control.Monad
import System.Directory.Recursive
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath.Posix
import CabalUtil
import Data.Maybe (Maybe(Nothing))
import MonadUtils

pprTid :: TargetId -> String
pprTid (TargetModule mName) = "TargetModule " ++ moduleNameString mName
pprTid (TargetFile fp _) = "TargetFile " ++ fp

pprDynFlags :: DynFlags -> IO ()
pprDynFlags dflags = do
  let docMaker = showSDoc dflags
  print (rtsOptsEnabled dflags)
  mapM_ (putStrLn . moduleNameString) (pluginModNames dflags)
  print (null (packageFlags dflags))
  mapM_ (putStrLn . docMaker . ppr) (packageFlags dflags)
  -- mapM_ print (compilerInfo dflags)

typecheckSources :: [String] -> String -> Ghc (TypecheckedSource)
typecheckSources targets moduleName = do
  liftIO (putStrLn $ "Typechecking module " ++ moduleName)
  targets' <- mapM (\t -> guessTarget t Nothing) targets
  dflags <- getSessionDynFlags
  let docMaker = showSDoc dflags
  -- liftIO (mapM_ (putStrLn . pprTid . targetId) (targets'))
  setTargets targets'
  let mName = mkModuleName moduleName
  sStatus <- load $ LoadAllTargets
  modSum <- getModSummary mName
  shown <- showModule modSum
  p <- parseModule modSum
  t <- typecheckModule p
  return ( tm_typechecked_source t )
typecheckSource :: String -> String -> Ghc ( TypecheckedSource )
typecheckSource targetFile moduleName =
  typecheckSources [targetFile] moduleName

spanToLineInserts :: RealSrcSpan -> String -> [(Int, String)]
spanToLineInserts span ins = do
  let s = (srcSpanStartCol span)-1
  let e = (srcSpanEndCol span)-1
  [(s, "("), (s, ins), (e, ")")]


moduleNameFromSource :: String -> Maybe String
moduleNameFromSource source = do
  let regex = "module ([A-Za-z\\.]+)( \\((\n|.)*?\\))? where"
  let (beforeText, match, afterText, subMatches) = (source =~ regex) :: (String, String, String, [String])
  if match == "" then
    Nothing
  else
    Just ( head subMatches )

typeAnnotatePackage :: String -> IO ( [Maybe String] )
typeAnnotatePackage packageDir = do
  allFs <- getFilesRecursive packageDir
  let cabalF = head (filter (isSuffixOf ".cabal") allFs)
  initialDFlags <- runGhc (Just libdir) getSessionDynFlags
  cabalDynFlagsEither <- cabalSpecToDynFlags initialDFlags cabalF
  dFlags <- case cabalDynFlagsEither of
                  Left err -> do
                    putStrLn $ show err
                    return Nothing
                  Right cabalDynFlags -> return $ Just cabalDynFlags
  let possiblePkgCacheFs = (filter (\f -> (takeFileName f == "package.cache") && (isInfixOf "packagedb" f)) allFs)
  let pkgCacheDirMb = if null possiblePkgCacheFs then Nothing else Just (takeDirectory (head possiblePkgCacheFs))
  if (pkgCacheDirMb == Nothing) then (putStrLn "No package cache dir found!") else print pkgCacheDirMb
  let includeDir = packageDir ++ "/include"
  includeDirExists <- (doesDirectoryExist includeDir)
  let includeDirList = if includeDirExists then [includeDir ++ "/"] else []
  let possibleSrcDirs = ["/src", ""]
  possibleSrcDirs <- filterM (doesDirectoryExist) (map (\d -> packageDir ++ d) possibleSrcDirs)
  let srcDir = (head possibleSrcDirs) ++ "/"
  print includeDirList
  print includeDir
  let allHsFs = filter (\f -> isSuffixOf ".hs" f && takeBaseName f /= "Setup" &&
        not (isInfixOf "build" f || isInfixOf "test" f ||  isInfixOf "bench" f)) allFs
  let allHsPaths = catMaybes (map (stripPrefix srcDir) allHsFs)
  let modNames = map (\f -> intercalate "." (splitDirectories (dropExtension f))) allHsPaths
  mapM (\tup -> typeAnnotateModuleInSourcesFull (dFlags) pkgCacheDirMb includeDirList allHsFs (fst tup) (snd tup)) (zip allHsFs modNames)

typeAnnotateModuleInSources :: [String] -> String -> String -> IO ( Maybe String )
typeAnnotateModuleInSources = typeAnnotateModuleInSourcesFull Nothing Nothing []

typeAnnotateDecls :: (SDoc -> String) -> String -> LHsBinds GhcTc -> Ghc ( Maybe String )
typeAnnotateDecls docMaker fileContents binds = do
  let fileLines = lines fileContents
  hsc_env <- getSession
  bindTypeLocsMon <- mapM ( ( typeBindLocs hsc_env ) . unpackLocatedData ) ( bagToList binds )
  let unsafeBindTypeLocs = filter ( isOneLineSpan . fst ) ( concat bindTypeLocsMon )
  let bindTypeLocs = [ (rss, t) | (RealSrcSpan rss, t) <- unsafeBindTypeLocs ]
  let sortedTypeLocs = sortBy (\x y -> compare (fst x) (fst y)) bindTypeLocs
  let lineLocs = map ( srcLocLine . realSrcSpanStart . fst ) sortedTypeLocs
  -- let groupedByLineTypeLocs = groupBy (\x y -> (srcSpanStartLine( fst x )) == (srcSpanStartLine (fst x))) sortedTypeLocs
  let lineInserts = map (\x -> ((srcSpanStartLine ( fst x), spanToLineInserts (fst x) (docMaker (ppr (snd x)))))) sortedTypeLocs
  let lineInsertsPerLine = map (\i -> (
            concatMap snd (filter (\x -> fst x == i + 1) lineInserts)
          )) [0..(length fileLines - 1)]
  let insertedLines = zipWith (insertMultiple) fileLines lineInsertsPerLine
  let commentedLines = concatMap (\x -> if fst x == snd x then [snd x] else ["--" ++ fst x, snd x]) (zip insertedLines fileLines)
  return $ Just $ intercalate "\n" commentedLines

typeAnnotateModuleInSourcesFull :: (Maybe DynFlags) -> Maybe String -> [String] -> [String] -> String -> String -> IO ( Maybe String )
typeAnnotateModuleInSourcesFull initialDflagsMb pkgCacheDirMb includeDirs moduleFiles targetFile moduleName =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    fileContents <- readFile targetFile
    runGhc (Just libdir) $ do
      dflags <- case initialDflagsMb of
        Just df -> return df
        Nothing ->  getSessionDynFlags
      let includes = includePaths dflags
      let includes' = addQuoteInclude includes includeDirs
      let generalFlags' = (EnumSet.insert Opt_HideAllPackages (generalFlags dflags))
      let packageDBFlags' = case pkgCacheDirMb of
            Just pkgCacheDir -> [PackageDB (PkgConfFile pkgCacheDir)]
            Nothing -> packageDBFlags dflags
      let dflags' = dflags{
        includePaths = includes',
        -- generalFlags = generalFlags'
        packageDBFlags = packageDBFlags'
        -- ignorePackageFlags = ((IgnorePackage "Prelude"):(ignorePackageFlags dflags))
      }
      setSessionDynFlags (dopt_set dflags' Opt_D_dump_rn )
      let docMaker = showSDoc dflags'
      liftIO ( putStrLn ("On file " ++ targetFile))
      typecheckedSource <- typecheckSources moduleFiles moduleName
      typeAnnotateDecls docMaker fileContents typecheckedSource

typeAnnotateSource :: String -> String -> IO ( Maybe String )
typeAnnotateSource targetFile mName = typeAnnotateModuleInSources [targetFile] targetFile mName

typeIpBindLocs ::  (MonadIO m) => HscEnv -> LIPBind GhcTc -> m ( [(SrcSpan, Type)] )
typeIpBindLocs hsc_env lipBind = do
  case (unpackLocatedData lipBind) of
    IPBind _ _ lexpr -> typeExprLocs hsc_env lexpr
    _   -> return []
typeLocalBindsLocs :: (MonadIO m) => HscEnv -> HsLocalBinds GhcTc -> m ( [(SrcSpan, Type)] )
typeLocalBindsLocs hsc_env localBinds = do
  case localBinds of
    HsValBinds _ deeperValBinds -> do
      case deeperValBinds of
        ValBinds _ lvalBindsBag sigs -> do
          sublocs <- mapM ( (typeBindLocs hsc_env) . unpackLocatedData ) (bagToList lvalBindsBag)
          return $ concat sublocs
        XValBindsLR xxValBinds -> do
          case xxValBinds of
            NValBinds bindList sigs -> do
              let sublists = concatMap ( bagToList . snd ) (bindList)
              tmp <- mapM ( (typeBindLocs hsc_env) . unpackLocatedData ) sublists
              return $ concat tmp
    HsIPBinds _ lIpBinds -> do
        case lIpBinds of
          IPBinds _ bindList -> do
            tmp <- mapM (typeIpBindLocs hsc_env) bindList
            return $ concat tmp
          _ -> return []
    _ -> return []
typeStmtLocs :: (MonadIO m) => HscEnv -> ExprLStmt GhcTc -> m ( [(SrcSpan, Type)] )
typeStmtLocs hsc_env lStmt = do
  let stmt = unpackLocatedData lStmt
  let stmtLoc = unpackLocatedLocation lStmt
  case stmt of
    BodyStmt _ body sExpr1 sExpr2 -> do
      typeExprLocs hsc_env body
    LetStmt _ locLocalBinds -> do
      let localBinds = unpackLocatedData locLocalBinds
      typeLocalBindsLocs hsc_env localBinds
    LastStmt _ body _ sExpr -> do
      typeExprLocs hsc_env body
    _ -> return []

typeExprLocs :: (MonadIO m) => HscEnv -> LHsExpr GhcTc -> m ( [(SrcSpan, Type)] )
typeExprLocs hsc_env lexpr = do
  let expr = unpackLocatedData lexpr
  let loc = unpackLocatedLocation lexpr
  tMb <- lexprTypeWithEnv hsc_env lexpr
  let initialList = case tMb of
                      Just t -> [(loc, t)]
                      Nothing -> []
  let subExprs = case expr of
                    HsApp _ e1 e2 -> [e1, e2]
                    HsAppType _ e1 _ -> [e1]
                    OpApp _ e1 e2 e3 -> [e1, e2, e3]
                    NegApp _ e1 _ -> [e1]
                    HsPar _ e1 -> [e1]
                    HsLet _ _ e1 -> [e1]
                    ExplicitList _ _ l -> l
                    _ -> []
  subExprTypes <- mapM (lexprTypeWithEnv hsc_env )subExprs
  let subExprLocs = map unpackLocatedLocation subExprs

  let zippedMb = (zip subExprLocs subExprTypes)
  let retMb = map ( \x -> case (snd x) of
                          Just xin -> Just (fst x, xin)
                          Nothing -> Nothing
                ) zippedMb
  complexSubExprs <- case expr of
                          HsDo _ _ lStmtList -> do
                            tmp <- mapM (typeStmtLocs hsc_env) ( unpackLocatedData lStmtList )
                            return $ concat tmp
                          HsLet _ localBinds _ -> do
                            tmp <- mapM (typeLocalBindsLocs hsc_env) ( localBinds )
                            return $ unpackLocatedData tmp
                          _ -> return []
  return $ initialList ++ (catMaybes retMb) ++ (complexSubExprs)


lexprStr :: (MonadIO m) => (SDoc -> String) -> HscEnv -> LHsExpr GhcTc -> m ( Maybe String )
lexprStr docMaker hsc_env lexpr = do
  typeLocs <- typeExprLocs hsc_env lexpr
  return $ Just $ show $ map ( docMaker . ppr . snd ) typeLocs
  -- return $ Just $ docMaker $ ppr lexpr

typeBindLocs :: (MonadIO m) => HscEnv -> HsBindLR GhcTc GhcTc -> m ( [(SrcSpan, Type)] )
typeBindLocs hsc_env bind = do
  case bind of
    FunBind fun_ext _ fun_matches _ _ -> do
      let matches = map ( m_grhss . unpackLocatedData ) ( unpackLocatedData $ mg_alts fun_matches )
      let lMatchGuardedRHS = concatMap ( grhssGRHSs ) matches
      let matchGuardedRHS = map unpackLocatedData lMatchGuardedRHS
      let exprs = ( map (\x -> case x of
                                  GRHS _ lstmt exprs -> exprs
                        ) matchGuardedRHS )
      exprTypes <- mapM (typeExprLocs hsc_env) exprs
      return ( concat exprTypes )
    -- PatBind _ _ _ _ -> Just $ docMaker (ppr bind)
    VarBind _ _ var_rhs _ -> do
      typeExprLocs hsc_env var_rhs
    AbsBinds _ abs_tvs abs_ev_vars abs_exports abs_ev_binds abs_binds _ -> do
      subBindStrsMb <- mapM ( (typeBindLocs hsc_env) . unpackLocatedData ) ( bagToList abs_binds )
      return $ concat subBindStrsMb
    _ -> return []

lexprTypeWithEnv :: (MonadIO m) => HscEnv -> LHsExpr GhcTc -> m ( Maybe Type )
lexprTypeWithEnv hsc_env lexpr = do
  exprAndMsgs <- liftIO ( deSugarExpr hsc_env lexpr )
  let exprMb = snd exprAndMsgs
  case exprMb of
    Just coreExpr -> do
      let t = CoreUtils.exprType coreExpr
      return ( Just $ t )
    Nothing -> do
      return Nothing

getHsBindLRName :: (SDoc -> String) -> HsBindLR GhcTc GhcTc -> Maybe String
getHsBindLRName docMaker bind = case bind of
  -- FunBind _ _ _ _ _ -> Just $ docMaker (ppr bind)
  -- PatBind _ _ _ _ -> Just $ docMaker (ppr bind)
  -- VarBind _ _ _ _ -> Just $ docMaker (ppr bind)
  AbsBinds _ abs_tvs abs_ev_vars abs_exports abs_ev_binds abs_binds _ -> do
    let subVals = map abe_mono abs_exports
    Just ( intercalate "\n" (map ( docMaker . ppr ) subVals ) )
  _ -> Nothing

