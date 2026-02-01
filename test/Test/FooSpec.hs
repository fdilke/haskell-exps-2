module Test.FooSpec where

import Test.Hspec

appendMe :: String -> String
appendMe s = s ++ "3"

spec :: Spec
spec = do
    describe "appendMe" $ do
        it "appends 3" $
            appendMe "x" `shouldBe` "x3"

