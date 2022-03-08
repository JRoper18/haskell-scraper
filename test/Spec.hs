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

main :: IO ()
main = do
    dflags <- runGhc (Just libdir) $ do
        getSessionDynFlags
    let docMaker = showSDoc dflags
    let testF = "./test/resources/f.hs"
    parsedSourceOrErr <- parseSource testF
    tcedSource <- typecheckSource testF
    typedBindsMb <- runGhc (Just libdir) $ do
        hsc_env <- getSession
        
        mapM ( ( getHsBindLRType docMaker ) . unpackLocatedData ) ( bagToList tcedSource )
    let typedBinds = catMaybes typedBindsMb
    dsugar <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        target <- guessTarget testF Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "A"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        return $ mg_binds $ dm_core_module d
    putStrLn $ docMaker $ ppr $ dsugar
    let tcStr = docMaker ( ppr ( tcedSource ) )
    -- putStrLn tcStr
    mapM_ putStrLn typedBinds
    hspec $ do
        describe "parsing" $ do
            it "should parse the source" $ do
                case parsedSourceOrErr of
                    Right parsedSource -> do
                        let decls = hsmodDecls ( unpackLocatedData (parsedSource) )
                        let namedDecls = getNamedDecls docMaker decls
                        let showableDecls = ( mapFst ( showDecl docMaker ) namedDecls )
                        -- print showableDecls
                        return ()
                    Left res -> printErrMessages res
                ( isRight parsedSourceOrErr ) `shouldBe` True
        -- describe "typechecking" $ do
        --     it "should typecheck files" $ do

                


