module CabalUtil where

import DynFlags
import Distribution.Fields
import Data.String
import Data.Maybe
import Data.Char
import Text.Parsec.Error
import GHC.LanguageExtensions
import qualified Data.ByteString as B
import Data.ByteString.Char8 as C8 (pack, unpack)

cabalSpecToDynFlags :: DynFlags -> FilePath -> IO ( Either ParseError DynFlags )
cabalSpecToDynFlags prevFlags specPath = do
    contents <- B.readFile specPath
    let fieldsEither = readFields contents
    case fieldsEither of
        Left ex -> return $ Left ex
        Right v -> do
            let libFields = cabalLibrary v
            let otherExtsMb = mapMaybe (getFieldlinesWithName "other-extensions") libFields ++ mapMaybe (getFieldlinesWithName "default-extensions") libFields
            let otherExts = if null otherExtsMb
                then []
                else do
                    let contentsFiltered = map (words . filter (/= ',') . fieldlineContents) (concat otherExtsMb)
                    concat contentsFiltered
            -- putStrLn "Extension strings found:"
            -- mapM_ putStrLn otherExts
            -- mapM_ print (mapMaybe extFromStr otherExts)
            -- putStrLn "Done listing extension strings"
            let extFlags = setExtensions prevFlags otherExts
            let cabalIncludePaths = map fieldlineContents (concat (mapMaybe (getFieldlinesWithName "other-extensions") libFields))
            let lastDynFlags = extFlags {
                includePaths = addQuoteInclude (includePaths extFlags) cabalIncludePaths
            }
            return $ Right $ lastDynFlags

extFromStr :: String -> Maybe Extension
extFromStr "CPP" = Just Cpp
extFromStr "PatternGuards" = Just PatternGuards
extFromStr "DefaultSignatures" = Just DefaultSignatures
extFromStr "FlexibleContexts" = Just FlexibleContexts
extFromStr "FlexibleInstances" = Just FlexibleInstances
extFromStr "GADTs" = Just GADTs
extFromStr "MultiParamTypeClasses" = Just MultiParamTypeClasses
extFromStr "TypeOperators" = Just TypeOperators
extFromStr "BangPatterns" = Just BangPatterns
extFromStr "DeriveDataTypeable" = Just DeriveDataTypeable
extFromStr "TypeFamilies" = Just TypeFamilies
extFromStr "MagicHash" = Just MagicHash
extFromStr "UnboxedTuples" = Just UnboxedTuples
extFromStr "UnliftedFFITypes" = Just UnliftedFFITypes
extFromStr _ = Nothing

cmpBsStrLower :: String -> B.ByteString -> Bool
cmpBsStrLower str bs = do
    map toLower str == map toLower (unpack bs)
setExtensions :: DynFlags -> [String] -> DynFlags
setExtensions dflags extStrs = do
    let exts = mapMaybe extFromStr extStrs
    let dflags' = Prelude.foldl xopt_set dflags exts
    dflags'

fieldlineContents :: FieldLine a -> String
fieldlineContents (FieldLine _ bs) = unpack bs

getFieldlinesWithName :: String -> Field a -> Maybe ([FieldLine a])
getFieldlinesWithName name (Field (Name _ nameBs) fieldLines) = if cmpBsStrLower name nameBs then Just fieldLines else Nothing
getFieldlinesWithName _ _ = Nothing

getSubfieldsWithName :: String -> Field a -> Maybe [Field a]
getSubfieldsWithName name (Section (Name _ nameBs) _ subFields) = if cmpBsStrLower name nameBs then Just subFields else Nothing
getSubfieldsWithName _ _ = Nothing


cabalLibrary :: [Field a] -> [Field a]
cabalLibrary ast = head $ mapMaybe (getSubfieldsWithName "library") ast



-- queryString :: Value -> [Key] -> Maybe String
-- queryString (String txt) [] = Just $ unpack txt
-- queryString (Object obj) (key:rest) = do
--     let subValMb = KM.lookup key obj
--     case subValMb of
--         Nothing -> Nothing
--         Just subVal -> queryString subVal rest
-- queryString _ _ = Nothing

-- queryStringArray :: Value -> [Key] -> Maybe ( Vector String )
-- queryStringArray (Array arr) [] = Just $ Data.Vector.catMaybes $ Data.Vector.map (\v -> queryString v []) arr
-- queryStringArray (Object obj) (key:rest) = do
--     let subValMb = KM.lookup key obj
--     case subValMb of
--         Nothing -> Nothing
--         Just subVal -> queryStringArray subVal rest
-- queryStringArray _ _ = Nothing