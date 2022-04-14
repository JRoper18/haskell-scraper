{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module LibUtil where

import GHC
import Data.List
import GHC.Generics
import Data.Data
import Json
import Data.Generics.Text
import Text.ParserCombinators.ReadP
import Data.Generics
import Control.Monad (mzero)
import Text.Read.Lex (hsLex)
import FastString
import OccName (mkVarOcc, OccName, occNameString)
import Bag
import GHC.Paths (libdir)
import GhcPlugins (SDoc, nameUnique, nameOccName, mkSystemName, vanillaIdInfo, IdDetails( VanillaId ))
import Outputable (showSDoc)
import ErrUtils (ErrorMessages)
import System.IO (hPutStrLn, stderr)
import Unique (unpkUnique, mkUnique)
import IfaceSyn (ShowHowMuch(ShowSome))
import qualified Data.ByteString as BS
import qualified Data.Text as TXT
import Var
import Type
import TcEvidence
import Unique -- https://hackage.haskell.org/package/ghc-8.10.7/docs/TcEvidence.html#t:TcEvBinds
import TyCon
import ConLike
import DataCon
import DynFlags

srcSpanMacro = "{Span}"
faststringMacro = "{FStr}"
occNameMacro = "{OcN}"
nameMacro = "{Nm}"
moduleNameMacro = "{Mnm}"
varMacro = "{Vr}"
tcEvBindsMacro = "{TcEv}"
tyConMacro = "{TC}"
conLikeMacro = "{CL}"

unpackLocatedData :: GHC.Located( p ) -> p
unpackLocatedData (L l m) = m

unpackLocatedLocation :: GHC.Located( p ) -> GHC.SrcSpan
unpackLocatedLocation (L l m) = l

insertAtLoc :: String -> Int -> String -> String
insertAtLoc initial loc ins = do
    let (l, r) = splitAt loc initial
    l ++ ins ++ r


insertMultiple :: String -> [(Int, String)] -> String
insertMultiple initial locsAndInts = do
    let sLocs = sortBy (\x y -> compare (fst y) (fst x) ) locsAndInts
    let allIns = foldl (uncurry . insertAtLoc) initial sLocs
    allIns

greadAbstract :: Data a => ReadS a
greadAbstract = readP_to_S greadAbstract'

  -- Helper for recursive read
greadAbstract' :: Data a' => ReadP a'
greadAbstract' = extR(extR(extR(extR(extR(extR(extR (extR (extR (extR (extR (extR allButString 
    srcSpanReadP) 
    stringCase) 
    faststringCase) 
    occNameCase) 
    tcBagCase) 
    parseBagCase)
    nameCase)
    moduleNameCase)
    varCase)
    tcEvBindsCase)
    tyConCase)
    conLikeCase where

    -- A specific case for strings
    -- boolCase :: ReadP Bool = do
    --     str <- choice [string "False", string "True"]
    --     if str == "True" then return True else return False


    byteStringCase :: ReadP BS.ByteString
    byteStringCase = do
        string "({BS})"
        return $ BS.empty
    faststringCase :: ReadP FastString
    faststringCase = do
        openParen
        string faststringMacro
        name <- stringCase
        closeParen
        return $ mkFastString name

    occNameCase :: ReadP OccName
    occNameCase = do
        openParen
        string occNameMacro
        name <- stringCase
        closeParen
        return $ mkVarOcc name

    nameCase :: ReadP GHC.Name
    nameCase = do
        openParen
        string nameMacro
        uniqCh <- get
        uniqIntStr <- skipUntil (== '|')
        char '|'
        occName <- occNameCase
        closeParen
        return $ mkSystemName (mkUnique uniqCh (read uniqIntStr)) occName

    varCase :: ReadP Var
    varCase = do
        openParen 
        string varMacro
        name <- nameCase
        let typ = mkTyConTy (funTyCon)   
        closeParen
        return $ mkLocalVar VanillaId name typ vanillaIdInfo

    tcEvBindsCase :: ReadP TcEvBinds
    tcEvBindsCase = do
        openParen
        string tcEvBindsMacro
        closeParen
        return $ emptyTcEvBinds

    tyConCase :: ReadP TyCon.TyCon
    tyConCase = do
        openParen
        string tyConMacro
        name <- nameCase
        closeParen
        let kind = liftedTypeKind
        return $ mkPrimTyCon name [] kind []

    conLikeCase :: ReadP ConLike
    conLikeCase = do
        openParen
        string conLikeMacro
        name <- nameCase
        closeParen
        let typ = mkTyConTy (funTyCon)   
        let id = mkLocalVar VanillaId name typ vanillaIdInfo
        let kind = liftedTypeKind
        let tyCon = mkPrimTyCon name [] kind []
        -- Fuck me that's stupid. This is so bad but I don't care. 
        let dataCon = mkDataCon name False name [] [] [] [] [] [] [] [] typ NoRRI tyCon fIRST_TAG [] id NoDataConRep
        return $ RealDataCon dataCon

    moduleNameCase :: ReadP GHC.ModuleName 
    moduleNameCase = do
        openParen
        string moduleNameMacro
        name <- skipUntil (== ')')
        closeParen
        return $ mkModuleName name

    stringCase :: ReadP String
    stringCase = readS_to_P reads

    -- Determine result type
    myDataType = dataTypeOf (getArg allButString)
     where
      getArg :: ReadP a'' -> a''
      getArg = undefined

    -- The generic default for gread
    allButString = do
        -- Drop "  (  "
        openParen
                -- Do the real work

        x <- choice [buildGeneric, greadAbstract']
                        -- Drop "  )  "
        closeParen
        return x

    -- Turn string into constructor driven by the requested result type,
    -- failing in the monad if it isn't a constructor of this data type

    buildGeneric = do
        str <- parseConstr            -- Get a lexeme for the constructor
        con  <- str2con str            -- Convert it to a Constr (may fail)
        fromConstrM greadAbstract' con -- Read the children

    str2con :: String -> ReadP Constr
    str2con = maybe mzero return
            . readConstr myDataType

    -- Get a Constr's string at the front of an input string
    parseConstr :: ReadP String
    parseConstr =
               string "[]"     -- Compound lexeme "[]"
          <++  string "()"     -- singleton "()"
          <++  infixOp         -- Infix operator in parantheses
          <++  hsLex           -- Ordinary constructors and literals

    -- Handle infix operators such as (:)
    infixOp :: ReadP String
    infixOp = do c1  <- char '('
                 str <- munch1 (not . (==) ')')
                 c2  <- char ')'
                 return $ [c1] ++ str ++ [c2]

removeBetweenDelims :: TXT.Text -> TXT.Text -> TXT.Text -> TXT.Text
removeBetweenDelims startDelim endDelim txt = do
    let afterStarts = TXT.splitOn startDelim txt
    if length afterStarts == 1 then txt else do
        let afterStartsAndEnds = map (TXT.concat . tail . TXT.splitOn endDelim) afterStarts
        TXT.concat ((head afterStarts) : afterStartsAndEnds)

removeComments :: String -> String
removeComments str = TXT.unpack $ removeBetweenDelims (TXT.pack "/*") (TXT.pack "*/") (TXT.pack str)

comment :: String -> String
comment str = "/*" ++ str ++ "*/"

readData :: Data a => String -> Maybe a
readData s = do
    let preProcessed = removeComments s
    let readS = greadAbstract preProcessed
    if null readS then
        Nothing
    else
        Just $ fst $ head readS
showData :: Data a => a -> String
showData d = do
    gshowAbstract d
    -- let constr = show $ toConstr d
    -- let subShown = gmapQ (showData) d
    -- case cast d of
    --     Just (srcSpan :: SrcSpan) -> "(" ++ srcSpanMacro ++ ")"
    --     -- show $ renderJSON $ json srcSpan
    --     Nothing -> do
    --         -- gshow d
    --         if null subShown then
    --             "(" ++ constr ++ ")"
    --         else
    --             "(" ++ constr ++ " " ++ (intercalate " " subShown) ++ ")"

gshowAbstract :: Data a => a -> String
gshowAbstract x = gshowsAbstract x ""

gshowsAbstract :: Data a => a -> ShowS

-- This is a prefix-show using surrounding "(" and ")",
-- where we recurse into subterms with gmapQ.
gshowsAbstract = extQ(extQ(extQ(extQ(extQ(extQ(extQ(extQ(extQ(extQ(extQ generalCase 
    stringCase) 
    occNameCase) 
    srcSpanShowS) 
    fastStringCase ) 
    nameCase) 
    moduleNameCase)
    byteStringCase)
    varCase)
    tcEvBindsCase)
    tyConCase)
    conLikeCase where
    
    
    generalCase t = do
        showChar '('
        . (showString . showConstr . toConstr $ t)
        . (foldr (.) id . gmapQ ((showChar ' ' .) . gshowsAbstract) $ t)
        . showChar ')'

    stringCase = shows :: String -> ShowS

    occNameCase :: OccName -> ShowS
    occNameCase ocn = do
        showChar '(' . showString occNameMacro . stringCase (occNameString ocn) . showChar ')'
    
    srcSpanCase :: SrcSpan -> ShowS
    srcSpanCase ss = do
        showChar '(' . showString srcSpanMacro . showChar ')'
    
    fastStringCase :: FastString -> ShowS 
    fastStringCase fs = do
        showChar '(' . showString faststringMacro . stringCase (unpackFS fs) . showChar ')'
    
    nameCase :: GHC.Name -> ShowS 
    nameCase n = do
        let unpkU = unpkUnique $ nameUnique n 
        showChar '(' . showString nameMacro . showChar (fst unpkU) . showString (show (snd unpkU)) . showChar '|' . occNameCase (nameOccName n) . showChar ')'

    moduleNameCase :: GHC.ModuleName -> ShowS
    moduleNameCase mn = do
        showChar '(' . showString moduleNameMacro . showString (moduleNameString mn). showChar ')'
    
    byteStringCase :: BS.ByteString -> ShowS
    byteStringCase bs = showString "({BS})"

    varCase :: Var -> ShowS
    varCase v = showChar '(' . showString varMacro . nameCase (varName v) . showChar ')'

    tcEvBindsCase :: TcEvBinds -> ShowS
    tcEvBindsCase binds = showChar '(' . showString tcEvBindsMacro . showChar ')'

    tyConCase :: TyCon.TyCon -> ShowS
    tyConCase tc = showChar '(' . showString tyConMacro . nameCase (TyCon.tyConName tc) . showChar ')'

    conLikeCase :: ConLike -> ShowS
    conLikeCase cl = do
        showChar '(' . showString conLikeMacro . nameCase (conLikeName cl) . showChar ')'

    
makeDocMaker :: IO ( SDoc -> String )
makeDocMaker = do
    dflags <- runGhc (Just libdir) $ do
        getSessionDynFlags
    let dflags_gopt_set = foldl gopt_set dflags [Opt_SuppressUniques]  
    let dflags_gopt_unset = foldl gopt_unset dflags_gopt_set [Opt_PrintTypecheckerElaboration]  
    return $ showSDoc dflags_gopt_set

printErrMessages :: ErrorMessages -> IO ()
printErrMessages msgs = do
  let bagL = bagToList msgs
  let strs = map show bagL
  mapM_ ( hPutStrLn stderr ) strs 

srcSpanInnerShow :: SrcSpan -> String
srcSpanInnerShow = show
-- srcSpanInnerShow RealSrcSpan rss = show (RealSrcSpan rss)
-- srcSpanInnerShow UnhelpfulSrcSpan = show () 

srcSpanShowS :: SrcSpan -> ShowS
srcSpanShowS ss = do
    showChar '(' . showString srcSpanMacro . showString (srcSpanInnerShow ss) . showChar ')'

srcSpanReadP :: ReadP SrcSpan
srcSpanReadP = do
    openParen
    string srcSpanMacro
    skipUntil (== ')')
    closeParen
    return noSrcSpan

openParen :: ReadP ()
openParen = do
    skipSpaces                     -- Discard leading space
    _ <- char '('                  -- Parse '('
    skipSpaces                     -- Discard following space

closeParen :: ReadP ()
closeParen = do
    skipSpaces                     -- Discard leading space
    _ <- char ')'                  -- Parse ')'
    skipSpaces                     -- Discard following space

skipUntil :: (Char -> Bool) -> ReadP (String)
skipUntil pred = do
    s <- look
    skip s
    where
        skip :: String -> ReadP (String)
        skip (c:s) | (not . pred) c = do
            _ <- get;
            skipped <- skip s
            return $ [c] ++ skipped
        skip _                 = do return ""


bagCaseReadP :: Data a => ReadP (Bag a)
bagCaseReadP = do
    openParen
    string "{abstract:Bag"
    skipUntil (== '}')
    char '}'
    l <- greadAbstract'
    closeParen
    return $ listToBag l

tcBagCase :: ReadP (Bag(GenLocated SrcSpan (HsBindLR (GhcPass 'Typechecked) (GhcPass 'Typechecked)))) = bagCaseReadP
parseBagCase :: ReadP (Bag(GenLocated SrcSpan (HsBindLR (GhcPass 'Parsed) (GhcPass 'Parsed)))) = bagCaseReadP
