module Test.FastLaneSpec where

import FastLane
import Test.Hspec
-- import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
    describe "lambdas can" $ do
        it "mimic (+1) function" $
            ldaPlusOne 3 `shouldBe` 4
        it "express functions of several variables" $
            ldaAddThree 3 4 5 `shouldBe` 12