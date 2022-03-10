module LibUtil where

import GHC
import Data.List

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

