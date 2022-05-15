{-# LANGUAGE DeriveGeneric #-}

module Hackage where 

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Process
import GHC.IO.Handle.Text
import Data.Aeson (object, (.=), encode, decode, ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import System.Directory
import Data.Aeson.KeyMap (KeyMap, toList)
import Data.Aeson.Key as KEY
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL(writeFile)
import Data.ByteString.UTF8 as BSU(fromString)     -- from utf8-string
import GHC.Generics (Generic)
import Network.HTTP.Types.Header
import Data.List

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

type PackageVersion = [Int]
fromString :: String -> PackageVersion
fromString str = do
    let substrs = split '.' str
    let ints = map (read) substrs :: [Int]
    ints

toString :: PackageVersion -> String
toString pv = intercalate "." (map show pv)

data SourceContext = SourceContext {
  filePath :: String, 
  packageName :: String,
  packageVersion :: String,
  packageURL :: String
} deriving (Generic, Eq, Show, Read)

instance ToJSON SourceContext where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SourceContext



downloadPackageData :: Manager -> String -> IO ( SourceContext )
downloadPackageData manager pkgName = do
    let packageURL = "http://hackage.haskell.org/package/" ++ pkgName
    initialReq <- parseRequest packageURL
    let req = initialReq { requestHeaders = [(hAccept, (BSU.fromString "text/json"))]}
    rsp <- httpLbs req manager
    let rspStr = responseBody rsp 
    let initialCtx = SourceContext {
        filePath = "",
        packageName = pkgName,
        packageVersion = "",
        packageURL = packageURL
    }
    let rspJson = decode rspStr :: Maybe (KeyMap String)
    case rspJson of
        Just obj -> do 
            let keyVals = toList obj
            let normalVersions = map fst (filter (\kv -> (snd kv) == "normal") keyVals)
            let versionInts = map (Hackage.fromString . KEY.toString) normalVersions
            let latestNormal = maximum versionInts
            return $ initialCtx {packageVersion = (Hackage.toString latestNormal)}
        _ -> return initialCtx

downloadPackage :: FilePath -> Manager -> SourceContext -> IO ( )
downloadPackage outF manager sc = do
    let pkgName = packageName sc
    let downloadURL = "http://hackage.haskell.org/package/" ++ pkgName ++ "/" ++ pkgName ++ "-" ++ (packageVersion sc) ++ ".tar.gz"
    req <- parseRequest downloadURL
    rsp <- httpLbs req manager
    let code = statusCode $ responseStatus rsp
    putStrLn $ "The status code was: " ++ (show $ code)
    if code == 200 then do
        -- Save the body
        BL.writeFile outF (responseBody rsp)
    else do
        putStrLn $ "Error on package " ++ pkgName 
        return ()
