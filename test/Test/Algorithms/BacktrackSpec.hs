{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Algorithms.BacktrackSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Algorithms.Backtrack

spec :: Spec
spec = do
  describe "Backtracking" $ do
    it "gives 0 solutions when there are none" $
      solve @Bool @Int True (\_ -> []) `shouldBe` []