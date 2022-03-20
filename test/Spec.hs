{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Test.Hspec
import GHC.Paths ( libdir )
import GHC
import DynFlags
import Data.Tuple
import Data.Either
import Data.Data
import Data.Maybe
import Outputable
import GhcPlugins 
import Lib
import Bag
import LibUtil
import Typed
import Parsed
import Core
import Data.Aeson 
import Data.Generics



serializeId :: Data a => a -> Maybe a 
serializeId d = readData (showData d)

pshowSerializeId :: (Data a, Outputable a) => (a -> String) -> a -> Maybe String
pshowSerializeId pshow d = do
    let sid = serializeId d
    case sid of
        Just dat -> Just (pshow dat)
        Nothing -> Nothing
main :: IO ()
main = do
    dflags <- runGhc (Just libdir) $ do
        getSessionDynFlags
    let docMaker = showSDoc dflags
    let testF = "./test/resources/f.hs"
    fileContents <- readFile testF
    parsedSourceOrErr <- parseSource testF 
    tcedSource <- runGhc (Just libdir) $ do 
        typecheckSource testF "A"
    bindTypeLocs <- runGhc (Just libdir) $ do
        hsc_env <- getSession
        
        mapM ( ( typeBindLocs ) . unpackLocatedData ) ( bagToList tcedSource )
    dmod <- coreFromSource testF
    -- let tcStr = docMaker ( ppr ( tcedSource ) )
    -- putStrLn tcStr
    -- mapM_ putStrLn typedBinds
    hspec $ do
        describe "util" $ do
            it "should make nice data string representations" $ do
                let d1 = Just "1"
                -- putStrLn $ showData d1
                -- putStrLn $ gshow d1
                shouldBe (serializeId d1) (Just d1)
                let d3 = "testStr"
                shouldBe (serializeId d3) (Just d3)
                let sList = ["a", "b", "c"]
                shouldBe (serializeId sList) (Just sList)
            it "should work on weird types" $ do
                let d2 = noSrcSpan
                shouldBe (serializeId d2) (Just d2) 
                let d4 = (mkVarOcc "unknown occname")
                let pshowId1 = pshowSerializeId (docMaker . ppr) :: OccName -> Maybe String
                shouldBe (pshowId1 d4) (Just (docMaker (ppr d4)))

            it "should work on composite weirdness" $ do
                let d5 = mkRdrUnqual (mkVarOcc "unknown occname") :: RdrName
                let pshowId2 = pshowSerializeId (docMaker . ppr) :: RdrName -> Maybe String
                shouldBe (pshowId2 d5) (Just (docMaker (ppr d5)))
                let d6 = (L noSrcSpan d5)
                let pshowId3 = pshowSerializeId (docMaker . ppr) :: Located RdrName -> Maybe String
                shouldBe (pshowId3 d6) (Just (docMaker (ppr d6)))
                -- let bag = listToBag ["a", "b", "c"]
                -- putStrLn (showData bag)
                -- let pshowId4 = pshowSerializeId (docMaker . ppr)
                -- shouldBe (pshowId4 bag) (Just (docMaker (ppr bag)))
        describe "parsing" $ do
            it "should parse the source" $ do
                ( isRight parsedSourceOrErr ) `shouldBe` True
            case parsedSourceOrErr of
                Right parsedSource -> do
                    let decls = hsmodDecls ( unpackLocatedData (parsedSource) )
                    it "should get decl names" $ do
                        let namedDecls = getNamedDecls docMaker decls
                        let showableDecls = ( mapFst ( showDecl docMaker ) namedDecls )
                        shouldBe ["f", "f"] (map snd showableDecls)
                    it "should have valid str reprs" $ do
                        let idDecls = (map (readData . showData) decls) :: [Maybe (GHC.LHsDecl GhcPs)]
                        -- mapM_ (putStrLn . showData) decls
                         
                        -- mapM (putStrLn . (showDecl docMaker)) (catMaybes idDecls)
                        mapM_ (\t -> shouldBe True (isJust t)) idDecls
                        let cmpPprDecls = zip (map (showDecl docMaker) decls) (map (showDecl docMaker) (catMaybes idDecls))
                        mapM_ (putStrLn . fst) cmpPprDecls
                        mapM_ (putStrLn . snd) cmpPprDecls
                        mapM_ (\t -> shouldBe (fst t) (snd t)) cmpPprDecls
                Left res -> do
                    it "should not error on parsing" $ do
                        printErrMessages res
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
            it "should be able to do grab the module name" $ do
                let t2 = "module B (\
                \   f\
                \) where"
                shouldBe (moduleNameFromSource t2) (Just "B")
                shouldBe (moduleNameFromSource fileContents) (Just "A")
            it "should type files" $ do
                -- mapM_ (putStrLn . docMaker . ppr ) (concat bindTypeLocs)
                newFMb <- typeAnnotateSource testF
                shouldBe (isJust newFMb) True 
                case newFMb of
                    Just newF -> do
                        -- putStrLn newF
                        return ()
                    Nothing ->
                        return ()

