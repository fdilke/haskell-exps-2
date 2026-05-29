{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Algorithms.BacktrackSpec where

import Algorithms.Backtrack
import Test.Hspec

spec :: Spec
spec = do
  describe "Backtracking" $ do
    it "transforms a list of Eithers" $
      transformEithers [Left (1 :: Int), Right "a", Left 2, Left 3, Right "b"] `shouldBe` (([1, 2, 3] :: [Int]), ["a", "b"])
    it "gives 0 solutions when there are none" $
      solve @Bool @Int True (\_ -> []) `shouldBe` []