{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Test.Hspec
import GHC.Paths ( libdir )
import GHC
import DynFlags
import Data.Tuple
import Data.Either
import Data.Maybe
import Outputable
import GhcPlugins 
import Lib
import Bag
import LibUtil
import Typed
import Parsed
import Core

main :: IO ()
main = do
    dflags <- runGhc (Just libdir) $ do
        getSessionDynFlags
    let docMaker = showSDoc dflags
    let testF = "./test/resources/f.hs"
    parsedSourceOrErr <- parseSource testF
    tcedSource <- typecheckSource testF
    bindTypeLocs <- runGhc (Just libdir) $ do
        hsc_env <- getSession
        
        mapM ( ( typeBindLocs ) . unpackLocatedData ) ( bagToList tcedSource )
    dmod <- coreFromSource testF
    -- let tcStr = docMaker ( ppr ( tcedSource ) )
    -- putStrLn tcStr
    -- mapM_ putStrLn typedBinds
    hspec $ do
        describe "parsing" $ do
            it "should parse the source" $ do
                case parsedSourceOrErr of
                    Right parsedSource -> do
                        let decls = hsmodDecls ( unpackLocatedData (parsedSource) )
                        let namedDecls = getNamedDecls docMaker decls
                        let showableDecls = ( mapFst ( showDecl docMaker ) namedDecls )
                        shouldBe ["f", "f"] (map snd showableDecls)
                    Left res -> printErrMessages res
                ( isRight parsedSourceOrErr ) `shouldBe` True
        describe "coring" $ do
            it "should core files" $ do
                case parsedSourceOrErr of
                    Right parsedSource -> do
                        let modguts = dm_core_module dmod
                        let binds = mg_binds modguts
                        let bindsWithNames = map (\x -> (x, bindVar x)) binds
                        let decls = hsmodDecls ( unpackLocatedData (parsedSource) )
                        let declNames = map snd ( getNamedDecls docMaker decls )
                        let keptBindsWithNames = filter (\x -> elem (docMaker (ppr (snd x))) declNames) bindsWithNames
                        let keptBindNames = map ( docMaker . ppr . snd ) keptBindsWithNames
                        let keptBinds = map fst keptBindsWithNames
                        shouldBe ["f"] keptBindNames
                    Left res -> printErrMessages res
        describe "typing" $ do
            it "should type files" $ do
                mapM_ (putStrLn . docMaker . ppr ) (concat bindTypeLocs)




                


