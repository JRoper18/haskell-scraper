
module Core where

import GHC
import CoreUtils
import GHC.Paths ( libdir )
import DynFlags
import Outputable
import CoreSyn
import Var

coreFromSource :: String -> IO ( DesugaredModule )
coreFromSource targetFile =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      fileContents <- readFile targetFile
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        -- let dflags' = foldl xopt_set dflags
        --                     [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "A"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        return d        

bindVar :: Bind CoreBndr -> Var 
bindVar ( NonRec b _ ) = b
bindVar ( Rec l ) = fst $ head l

