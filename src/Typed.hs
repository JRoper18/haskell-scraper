module Typed where

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
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (isSpace)
import LibUtil
import CoreUtils
import Bag
import Desugar
import TcSMonad
import TcRnTypes
import System.IO (hPutStrLn, stderr)
import GHC.Unicode
import GhcPlugins
import Data.Aeson
import GHC.Generics

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

typeLocalBindsLocs :: HsLocalBinds GhcTc -> Ghc ( [(SrcSpan, Type)] )
typeLocalBindsLocs localBinds = do
  case localBinds of
    HsValBinds _ deeperValBinds -> do
      case deeperValBinds of
        ValBinds _ lvalBindsBag sigs -> do
          sublocs <- mapM ( typeBindLocs . unpackLocatedData ) (bagToList lvalBindsBag)
          return $ concat sublocs
        _ -> return []
    _ -> return []
typeStmtLocs :: ExprLStmt GhcTc -> Ghc ( [(SrcSpan, Type)] )
typeStmtLocs lStmt = do
  let stmt = unpackLocatedData lStmt
  let stmtLoc = unpackLocatedLocation lStmt
  case stmt of
    BodyStmt _ body sExpr1 sExpr2 -> do
      res <- typeExprLocs body
      return $ res
    LetStmt _ locLocalBinds -> do
      let localBinds = unpackLocatedData locLocalBinds
      localBindLocs <- typeLocalBindsLocs localBinds
      return $ localBindLocs
    LastStmt _ body _ sExpr -> do
      res <- typeExprLocs body
      return $ res
    _ -> return []

typeExprLocs :: LHsExpr GhcTc -> Ghc ( [(SrcSpan, Type)] )
typeExprLocs lexpr = do
  let expr = unpackLocatedData lexpr
  let loc = unpackLocatedLocation lexpr

  tMb <- lexprType lexpr
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
  subExprTypes <- mapM lexprType subExprs
  let subExprLocs = map unpackLocatedLocation subExprs
  
  let zippedMb = (zip subExprLocs subExprTypes)
  let retMb = map ( \x -> case (snd x) of 
                          Just xin -> Just (fst x, xin)
                          Nothing -> Nothing 
                ) zippedMb
  complexSubExprs <- case expr of
                          HsDo _ _ lStmtList -> do
                            tmp <- mapM typeStmtLocs ( unpackLocatedData lStmtList )
                            return $ concat tmp
                          HsLet _ localBinds _ -> do
                            tmp <- mapM typeLocalBindsLocs ( localBinds )
                            return $ unpackLocatedData tmp
                          _ -> return []
  return $ initialList ++ (catMaybes retMb) ++ (complexSubExprs)

  
lexprStr :: (SDoc -> String) -> LHsExpr GhcTc -> Ghc ( Maybe String )
lexprStr docMaker lexpr = do
  typeLocs <- typeExprLocs lexpr
  return $ Just $ show $ map ( docMaker . ppr . snd ) typeLocs
  -- return $ Just $ docMaker $ ppr lexpr

typeBindLocs :: HsBindLR GhcTc GhcTc -> Ghc ( [(SrcSpan, Type)] )
typeBindLocs bind = case bind of 
  FunBind fun_ext _ fun_matches _ _ -> do
    let matches = map ( m_grhss . unpackLocatedData ) ( unpackLocatedData $ mg_alts fun_matches )
    let lMatchGuardedRHS = concat ( map ( grhssGRHSs ) matches )
    let matchGuardedRHS = map unpackLocatedData lMatchGuardedRHS
    let exprs = ( map (\x -> case x of
                                GRHS _ lstmt exprs -> exprs
                      ) matchGuardedRHS )
    exprTypes <- mapM typeExprLocs exprs
    return ( concat exprTypes )
  -- PatBind _ _ _ _ -> Just $ docMaker (ppr bind)
  -- VarBind _ _ _ _ -> Just $ docMaker (ppr bind)
  AbsBinds _ abs_tvs abs_ev_vars abs_exports abs_ev_binds abs_binds _ -> do
    subBindStrsMb <- mapM ( typeBindLocs . unpackLocatedData ) ( bagToList abs_binds )
    return $ concat subBindStrsMb
  _ -> return []

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
      tMb <- lexprStr docMaker exprs
      return tMb
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
    return ( Just $ intercalate "\nmatch\n" matchResults )
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

