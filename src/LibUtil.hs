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
import OccName (mkVarOcc, OccName)
import Data.Aeson (Value(Bool))
import Bag

srcSpanMacro = "{abstract:SrcSpan}"
faststringMacro = "{abstract:FastString}"
occNameMacro = "{abstract:OccName}"

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
greadAbstract' = extR (extR (extR (extR (extR allButString srcSpanCase) stringCase) faststringCase) occNameCase) bagCase where

    -- A specific case for strings
    -- boolCase :: ReadP Bool = do
    --     str <- choice [string "False", string "True"]
    --     if str == "True" then return True else return False
        
    bagCase :: ReadP (Bag (GenLocated SrcSpan (HsBindLR (GhcPass 'Parsed) (GhcPass 'Parsed))))
    bagCase = do
        openParen
        string "{abstract:Bag"
        skipUntil (== '}')
        char '}'    
        l <- greadAbstract'
        closeParen
        return $ listToBag l
        
    srcSpanCase :: ReadP SrcSpan
    srcSpanCase = do
        openParen
        string srcSpanMacro
        closeParen
        return noSrcSpan
    faststringCase :: ReadP FastString
    faststringCase = do
        openParen
        string faststringMacro
        closeParen
        return $ mkFastString "errorStr"

    occNameCase :: ReadP OccName
    occNameCase = do
        openParen
        string occNameMacro
        closeParen
        return $ mkVarOcc "unknown occname"

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

    skipUntil :: (Char -> Bool) -> ReadP ()
    skipUntil pred = do
        s <- look
        skip s
        where
            skip (c:s) | (not . pred) c = do _ <- get; skip s
            skip _                 = do return ()
 
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


readData :: Data a => String -> Maybe a
readData s = do
    let readS = greadAbstract s
    if null readS then
        Nothing
    else
        Just $ fst $ head readS
showData :: Data a => a -> String
showData d = do
    gshow d
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
    