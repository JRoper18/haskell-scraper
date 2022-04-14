module IntermediatePlugin where

import GhcPlugins
import TcRnMonad
import TcRnTypes
import LibUtil
import Bag
import Typed
import GHC
import Outputable
import GHC.Paths ( libdir )
import System.Environment
import Data.List
import Data.Maybe
import qualified Data.Text as TXT
import Parsed

-- makeBindStr :: (SDoc -> String) -> String -> [(SrcSpan, Type)] -> String
-- makeBindStr docMaker bindStr typeL = do
--     let spanStrsAndTypes = mapSnd (pack . docMaker . ppr) (mapFst (\ss -> pack ((srcSpanShowS ss) "")) typeL)
--     let bindTxt = pack $ showData binds
--     let replaced = foldl (\l spanType -> replace (fst spanType) (snd spanType) l) bindTxt spanStrsAndTypes  
--     unpack replaced


makeOutF :: String -> ModSummary -> String
makeOutF suffix modsum = do
    let dflags = ms_hspp_opts modsum
    let docMaker = showSDoc dflags 
    "../../typed-results/" ++ docMaker (ppr (moduleName (ms_mod modsum))) ++ "." ++ suffix ++ ".txt"
parsedOutF = makeOutF "parsed"
typedOutF = makeOutF "typed"

parsedResAction :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedResAction _ modsum pmod = do
    let psource = hpm_module pmod
    let dflags = ms_hspp_opts modsum
    let docMaker = showSDoc dflags
    let declStrs = declStrsFromParsedSource docMaker psource
    let outF = parsedOutF modsum
    liftIO $ do
        print outF
        writeFile outF declStrs
    return pmod

typecheckResAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckResAction _ modsum tcenv = do
    let binds = filter typedBindIsImportant (map unpackLocatedData (bagToList (tcg_binds tcenv))) 
    hsc_env <- getTopEnv
    let dflags = ms_hspp_opts modsum
    let docMaker = showSDoc dflags 
    let outF = typedOutF modsum
    liftIO $ do
        -- let parsedF = parsedOutF modsum
        -- parsedFLines <- readFile parsedF
        -- let bindTxts = map (TXT.pack) (filter (/= "<|splitter|>") (lines parsedFLines))
        replaced <- mapM (bindToASTString docMaker hsc_env) binds
        -- let origTxts = map (docMaker . ppr) binds
        -- let finalStrs = map show (zip origTxts replaced)
        -- appendFile outF (intercalate "\nreplacements:\n" (map (\p -> unpack (fst p) ++ " " ++ unpack (snd p)) typeLocTxts))
        let wr = intercalate "\n<|splitter|>\n" replaced
        writeFile outF wr
        -- mapM_ (\t -> appendFile outF (fst t ++ "\n<splitter>\n" ++ show (snd t) ++ "\n\n\n")) (zip replaced safeTypeLocs)
    return tcenv


plugin :: Plugin
plugin = defaultPlugin {
    parsedResultAction = parsedResAction,
    typeCheckResultAction = typecheckResAction
}