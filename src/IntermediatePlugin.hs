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


makeOutF :: String -> ModSummary -> String
makeOutF suffix modsum = do
    let dflags = ms_hspp_opts modsum
    let docMaker = showSDoc dflags 
    "../../typed-results/" ++ docMaker (ppr (moduleName (ms_mod modsum))) ++ "." ++ suffix ++ ".txt"
parsedASTOutF = makeOutF "parsedAST"
simpleParsedOutF = makeOutF "parsed"
typedOutF = makeOutF "typedAST"

parsedResAction :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedResAction _ modsum pmod = do
    let psource = hpm_module pmod
    let dflags = ms_hspp_opts modsum
    let docMaker = showSDoc dflags
    let astDeclStrs = concatDeclStrsSimple $ treeDeclStrsFromParsedSource docMaker psource
    let astOutF = parsedASTOutF modsum
    let simpleDeclStrs = concatDeclStrsSimple $ plainDeclStrsFromParsedSource docMaker psource
    let simpleOutF = simpleParsedOutF modsum
    liftIO $ do
        print astOutF
        writeFile astOutF astDeclStrs
        writeFile simpleOutF simpleDeclStrs
    return pmod

typecheckResAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckResAction _ modsum tcenv = do
    let binds = filter typedBindIsImportant (map unpackLocatedData (bagToList (tcg_binds tcenv))) 
    hsc_env <- getTopEnv
    let dflags = ms_hspp_opts modsum
    let docMaker = showSDoc dflags 
    let outF = typedOutF modsum
    liftIO $ do
        replaced <- mapM (bindToASTString docMaker hsc_env) binds
        let wr = intercalate "\n<|splitter|>\n" replaced
        writeFile outF wr
    return tcenv


plugin :: Plugin
plugin = defaultPlugin {
    parsedResultAction = parsedResAction,
    typeCheckResultAction = typecheckResAction
}