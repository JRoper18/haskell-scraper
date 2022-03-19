{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LibUtil where

import GHC
import Data.List
import GHC.Generics
import Data.Data
import Json
import Data.Generics.Text

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


readData :: Data a => String -> Maybe a
readData s = do 
    let readS = gread s
    if null readS then
        Nothing 
    else 
        Just $ fst $ head readS
showData :: Data a => a -> String
-- showData (s::GHC.SrcSpan) = do
    
showData d = do
    gshow d
    -- let constr = show $ toConstr d
    -- let subShown = gmapQ (showData) d
    -- let srcSpanMb = dataCast1 SrcSpan d
    -- case srcSpanMb of
    --     Just srcSpan -> show $ renderJSON $ json srcSpan
    --     Nothing -> do
    --         if null subShown then
    --             constr
    --         else 
    --             constr ++ " (" ++ (intercalate ")(" subShown) ++ ") "