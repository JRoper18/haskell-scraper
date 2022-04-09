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

typecheckResAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckResAction _ modsum tcenv = do
    let binds = bagToList $ tcg_binds tcenv
    let bindTxts = map (TXT.pack . showData) binds
    hsc_env <- getTopEnv
    let outF = "./replaced.txt"
    liftIO $ do
        dflags <- runGhc (Just libdir) getSessionDynFlags
        let docMaker = showSDoc dflags
        unsafeTypeLocs <- mapM ( typeBindLocs hsc_env . unpackLocatedData ) binds
        let safeTypeLocs = map (mapSnd (TXT.pack . comment . docMaker . ppr) . realSpans) unsafeTypeLocs
        -- let spanStrsAndTypes = mapSnd (pack . docMaker . ppr) (mapFst (\ss -> pack ((srcSpanShowS ss) "")) (concat safeTypeLocs))
        -- (mapM_ (putStrLn . unpack . fst)) spanStrsAndTypes
        let replaced = zipWith (curry (\t -> TXT.unpack (uncurry replaceSpanInnersWithTypes t))) bindTxts safeTypeLocs
        -- appendFile outF (intercalate "\nreplacements:\n" (map (\p -> unpack (fst p) ++ " " ++ unpack (snd p)) typeLocTxts))
        let wr = intercalate "\n<splitter>\n" replaced
        appendFile outF wr
        -- mapM_ (\t -> appendFile outF (fst t ++ "\n<splitter>\n" ++ show (snd t) ++ "\n\n\n")) (zip replaced safeTypeLocs)
    return tcenv
plugin :: Plugin
plugin = defaultPlugin {
    typeCheckResultAction = typecheckResAction
}