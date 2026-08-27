{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{- HLINT ignore "Monoid law, right identity" -}
{- HLINT ignore "Monoid law, left identity" -}

module Test.GroupSpec where

import Group
import Test.Hspec
import Data.Foldable (for_)
import Debug.Trace (trace)
import Control.Exception (evaluate, throw, AssertionFailed (AssertionFailed))
import Data.List (find)

spec :: Spec
spec = do
  describe "CyclicGroup (Z/5Z)" $ do
    it "has correct orders" $
      cyclicGroup 6 \ @g -> do
        checkGroup @g 6
        orders @g @Int [
          0, 1, 2, 3, 4, 5
          ] `shouldBe` [1, 6, 3, 2, 3, 6]
        isAbelian @g `shouldBe` True

  describe "DihedralGroup D_12" $ do
    it "has correct orders" $
      dihedralGroup 6 \ @g -> do
        checkGroup @g 12
        orders @g @(Bool, Int) [
          (False, 0), (False, 1), (False, 2), (False, 3),
          (True, 0), (True, 1), (True, 2), (True, 3)
          ] `shouldBe` [1, 6, 3, 2, 2, 2, 2, 2]
        isAbelian @g `shouldBe` False

  describe "Permutation group S_4" $ do
    it "has correct orders" $
      permutationGroup 4 \ @g -> do
        checkGroup @g 24
        orders @g @[Int] [[0..3], [1, 0, 3, 2], [2, 1, 3, 0], [1, 2, 3, 0]] `shouldBe` [1, 2, 3, 4]
        isAbelian @g `shouldBe` False

  describe "Units mod 5, 6, 9" $ do
    it "has correct orders" $ do
      unitsMod 5 \ @g -> do
        checkGroup @g 4
        orders @g @Int [1, 2, 3, 4] `shouldBe` [1, 4, 4, 2]
        isAbelian @g `shouldBe` True
      unitsMod 6 \ @g -> do
        checkGroup @g 2
        orders @g @Int [1, 5] `shouldBe` [1, 2]
        isAbelian @g `shouldBe` True
      unitsMod 9 \ @g -> do
        checkGroup @g 6
        orders @g @Int [1, 2, 4, 5, 7, 8] `shouldBe` [1, 6, 3, 6, 3, 2]
        isAbelian @g `shouldBe` True

  describe "Metacyclic groups" $ do
    it "detects bad parameters" $ do
      evaluate (metacyclic 3 3 \ @_ -> 
        throw $ AssertionFailed "hurby burbly"
        ) `shouldThrow` anyException
    it "correctly calculated for order 6" $
      metacyclic 3 2 \ @g -> do
        checkGroup @g 6
        orders @g @(Int, Int) [(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1)] `shouldBe` [1, 3, 2, 2, 2, 3]
        isAbelian @g `shouldBe` False
    it "correctly calculated for order 21" $
      metacyclic 7 3 \ @g -> do
        checkGroup @g 21
        orders @g @(Int, Int) [(0, 0), (0, 1), (1, 0)] `shouldBe` [1, 7, 3]
        isAbelian @g `shouldBe` False

orders :: forall g p. (Group g, Switch g p) => [p] -> [Int]
orders ps = orderElement . switchFrom @g <$> ps

checkGroup :: forall g p. Group g => Int -> Expectation
checkGroup expectedOrder = do
  orderGroup @g `shouldBe` expectedOrder
  for_ (elementsList @g) $ \a -> do
    a <> mempty @g `shouldBe` a
    mempty @g <> a `shouldBe` a
    for_ (elementsList @g) $ \b -> do
      for_ (elementsList @g) $ \c -> do
        a <> (b <> c) `shouldBe` (a <> b) <> c

{-
  to do: calculations with subgroups
-}