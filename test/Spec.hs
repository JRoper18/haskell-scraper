{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Test.Hspec
import GHC.Paths ( libdir )
import GHC
import DynFlags
import Lib
import Data.Tuple
import Data.Either
import Data.Maybe
import Outputable

main :: IO ()
main = do
    dflags <- runGhc (Just libdir) $ do
        getSessionDynFlags
    let docMaker = showSDoc dflags
    parsedSourceOrErr <- parseSource "./test/resources/f.hs"
    hspec $ do
        describe "Lib" $ do
            it "should parse the source" $ do
                case parsedSourceOrErr of
                    Right parsedSource -> do
                        let decls = hsmodDecls ( unpackLocatedData (parsedSource) )
                        let namedDecls = getNamedDecls docMaker decls
                        let showableDecls = ( mapFst ( showDecl docMaker ) namedDecls )
                        print showableDecls
                    Left res -> printErrMessages res
                ( isRight parsedSourceOrErr ) `shouldBe` True


