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

replaceSpanInnersWithTypes :: TXT.Text -> [(SrcSpan, TXT.Text)] -> TXT.Text
replaceSpanInnersWithTypes txt typeLocs = do
    let txtLocs = mapFst (TXT.pack . srcSpanInnerShow) typeLocs
    let typeReplaced = replaceMany txtLocs txt
    filterInnerSpans (TXT.isPrefixOf (TXT.pack "/*")) typeReplaced

replaceMany :: [(TXT.Text, TXT.Text)] -> TXT.Text -> TXT.Text
replaceMany replacements origin =
    foldl (\l r -> uncurry TXT.replace r l) origin replacements

realSpans :: [(SrcSpan, Type)] -> [(SrcSpan, Type)]
realSpans unsafeLocs = [ (RealSrcSpan rss, t) | (RealSrcSpan rss, t) <- unsafeLocs ]

filterInnerSpans :: (TXT.Text -> Bool) -> TXT.Text -> TXT.Text
filterInnerSpans pred txt = do
    let spanMacroTxt = (TXT.pack srcSpanMacro)
    let srcSpans = TXT.splitOn spanMacroTxt txt
    let midSrcSpans = tail srcSpans
    if length srcSpans == 1 then txt else do
        let endSpanIdxs = map (fromMaybe 0 . TXT.findIndex (== ')')) midSrcSpans
        let spansAndIdxs = zip midSrcSpans endSpanIdxs
        let filtered = map (\tup ->
                if pred (TXT.take (snd tup) (fst tup)) then fst tup else TXT.drop (snd tup) (fst tup)
                ) spansAndIdxs
        let withoutMacros = (head srcSpans) : filtered
        TXT.intercalate spanMacroTxt withoutMacros

parsedOutF :: ModSummary -> String
parsedOutF modsum = do
    let dflags = ms_hspp_opts modsum
    let docMaker = showSDoc dflags 
    docMaker (ppr (moduleName (ms_mod modsum))) ++ ".parsed.txt"

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
    liftIO $ do
        let parsedF = parsedOutF modsum
        parsedFLines <- readFile parsedF
        let bindTxts = map (TXT.pack . showData) binds
        -- let bindTxts = map (TXT.pack) (filter (/= "<|splitter|>") (lines parsedFLines))
        let outF = "./typed.txt"
        let dflags = ms_hspp_opts modsum
        let docMaker = showSDoc dflags
        unsafeTypeLocs <- mapM ( typeBindLocs hsc_env ) binds
        let safeTypeLocs = map (mapSnd (TXT.pack . comment . docMaker . ppr) . realSpans) unsafeTypeLocs
        -- let spanStrsAndTypes = mapSnd (pack . docMaker . ppr) (mapFst (\ss -> pack ((srcSpanShowS ss) "")) (concat safeTypeLocs))
        -- (mapM_ (putStrLn . unpack . fst)) spanStrsAndTypes
        let replaced = zipWith (curry (\t -> TXT.unpack (uncurry replaceSpanInnersWithTypes t))) bindTxts safeTypeLocs
        -- appendFile outF (intercalate "\nreplacements:\n" (map (\p -> unpack (fst p) ++ " " ++ unpack (snd p)) typeLocTxts))
        let wr = intercalate "\n<|splitter|>\n" replaced
        appendFile outF wr
        -- mapM_ (\t -> appendFile outF (fst t ++ "\n<splitter>\n" ++ show (snd t) ++ "\n\n\n")) (zip replaced safeTypeLocs)
    return tcenv


plugin :: Plugin
plugin = defaultPlugin {
    parsedResultAction = parsedResAction,
    typeCheckResultAction = typecheckResAction
}