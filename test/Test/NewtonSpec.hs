module Test.NewtonSpec where

import Test.Hspec
import Text.Printf
import Newton

expected :: Fractional a => a
expected = 1.4142

spec :: Spec
spec = do
    describe "mySqrt" $ do
        it "can calculate the square root of 2" $
            let root = mySqrt (2 :: Double) in
                (printf "%.5f" root :: String) `shouldBe` "1.41421"
        it "can calculate the square root of 3" $
            let root = mySqrt (3 :: Double) in
                (printf "%.5f" root :: String) `shouldBe` "1.73205"


