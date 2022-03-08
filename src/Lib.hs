{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use join" #-}
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
import Data.Maybe
import Data.Data
import Data.Char (isSpace)
import Util
import CoreUtils
import Bag
import Desugar
import TcSMonad
import TcRnTypes
import System.IO (hPutStrLn, stderr)
import GHC.Unicode
import GhcPlugins

unpackLocatedData :: GHC.Located( p ) -> p
unpackLocatedData (L l m) = m

unpackLocatedLocation :: GHC.Located( p ) -> GHC.SrcSpan
unpackLocatedLocation (L l m) = l

showDecl :: (SDoc -> String) -> GHC.LHsDecl ( GHC.GhcPs ) -> String
showDecl docMaker decl = do
    let strList = docMaker ( ppr ( decl ) )
    strList

keepDecl :: GHC.LHsDecl ( GHC.GhcPs ) -> Bool
keepDecl decl = isJust ( getDeclIds (decl) )

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
      let groupedDeclStrs = map ( map ( showDecl docMaker ) ) groupedDecls 
      let finalStr = intercalate "\n<|splitter|>\n" ( map ( intercalate "\n" ) groupedDeclStrs )
      return ( Right finalStr )
    Left x -> do
      return (Left x)

typecheckSource :: String -> IO ( TypecheckedSource )
typecheckSource targetFile =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      fileContents <- readFile targetFile
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "A"
        p <- parseModule modSum
        t <- typecheckModule p
        return ( tm_typechecked_source t )




lexprType :: LHsExpr GhcTc -> Ghc ( Maybe Type ) 
lexprType lexpr = do
  hsc_env <- getSession
  exprAndMsgs <- liftIO ( deSugarExpr hsc_env lexpr )
  let exprMb = snd exprAndMsgs
  case exprMb of
    Just coreExpr -> do
      let t = CoreUtils.exprType coreExpr
      return ( Just $ t ) 
    Nothing -> do
      return Nothing


processGuardedMatch :: (SDoc -> String) -> LGRHS GhcTc (LHsExpr GhcTc) -> Ghc ( Maybe String )
processGuardedMatch docMaker match = do
  case (unpackLocatedData match) of
    GRHS _ lstmt exprs -> do
      t <- lexprType exprs
      return ( Just ( docMaker ( ppr t ) ) )
    _ -> return Nothing

processFunctionMatch :: (SDoc -> String) -> Match GhcTc ( LHsExpr GhcTc ) -> Ghc ( Maybe String )
processFunctionMatch docMaker match = do
  let rhs = m_grhss match
  guardRhs <- mapM ( processGuardedMatch docMaker ) ( grhssGRHSs rhs )
  let rhsStrs = catMaybes guardRhs
  return ( Just ( intercalate "functMatch" rhsStrs ) )

getHsBindLRType :: (SDoc -> String) -> HsBindLR GhcTc GhcTc -> Ghc ( Maybe String )
getHsBindLRType docMaker bind = case bind of 
  FunBind fun_ext _ fun_matches _ _ -> do
    let argTypes = docMaker $ ppr $ mg_arg_tys $ mg_ext fun_matches 
    let resType = docMaker $ ppr $ mg_res_ty $ mg_ext fun_matches 
    let matches = map ( unpackLocatedData ) ( unpackLocatedData $ mg_alts fun_matches )
    -- let matchPatterns = map m_pats matches
    matchStrsMb <- mapM ( processFunctionMatch docMaker  ) matches
    let matchResults = catMaybes matchStrsMb
    return ( Just $ intercalate "match" matchResults )
  -- PatBind _ _ _ _ -> Just $ docMaker (ppr bind)
  -- VarBind _ _ _ _ -> Just $ docMaker (ppr bind)
  AbsBinds _ abs_tvs abs_ev_vars abs_exports abs_ev_binds abs_binds _ -> do
    subBindStrsMb <- mapM ( ( getHsBindLRType docMaker ) . unpackLocatedData ) ( bagToList abs_binds )
    let subBindStrs = catMaybes subBindStrsMb
    return ( Just ( intercalate "split" subBindStrs ) )
  _ -> return ( Nothing )

getHsBindLRName :: (SDoc -> String) -> HsBindLR GhcTc GhcTc -> Maybe String
getHsBindLRName docMaker bind = case bind of 
  -- FunBind _ _ _ _ _ -> Just $ docMaker (ppr bind)
  -- PatBind _ _ _ _ -> Just $ docMaker (ppr bind)
  -- VarBind _ _ _ _ -> Just $ docMaker (ppr bind)
  AbsBinds _ abs_tvs abs_ev_vars abs_exports abs_ev_binds abs_binds _ -> do
    let subVals = map abe_mono abs_exports
    Just ( intercalate "\n" (map ( docMaker . ppr ) subVals ) ) 
  _ -> Nothing



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
        let filteredTxt = filter (\s -> isAscii s && (isSpace s || not ( isControl s))) fileContents
        let parseFullRes = parser filteredTxt dflags targetFile
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
        