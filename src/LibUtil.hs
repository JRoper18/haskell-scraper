{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module LibUtil where

import "aeson" Data.Aeson (ToJSON)
import "aeson" Data.Aeson.TH (deriveToJSON, defaultOptions)
import "base" GHC.Generics (Generic)
import "generic-deriving" Generics.Deriving.TH (deriveAll0)
import "th-reify-many" Language.Haskell.TH.ReifyMany
import GHC
import Data.List
import Language.Haskell.TH.Syntax
import GHC.Generics

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

-- $(do -- Don't bother trying to define a Generic instance for Int
--     let genPred n = n /= ''Int
--     exprTNameMb <- lookupTypeName "LHsBindLR GhcTc GhcTc"
--     case exprTNameMb of
--         Just exprTName -> do
--             genericInsts <- reifyManyWithoutInstances ''Generic [exprTName] genPred
--                             >>= traverse deriveAll0
--             toJSONInsts  <- reifyManyWithoutInstances ''ToJSON [exprTName] (const True)
--                             >>= traverse (deriveToJSON defaultOptions)
--             pure $ concat $ genericInsts ++ toJSONInsts
--         _ -> return []
--     )
        