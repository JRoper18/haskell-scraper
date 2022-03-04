import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Lib" $ do
        it "works" $ do
            1 `shouldBe` 2
            putStrLn "Test suite not yet implemented"
