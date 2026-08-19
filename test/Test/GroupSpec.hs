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

spec :: Spec
spec = do
  describe "CyclicGroup (Z/5Z)" $ do
    it "has correct orders" $
      cyclicGroup 6 \ @g -> do
        checkGroup @g 6
        orders @g [
          0, 1, 2, 3, 4, 5
          ] `shouldBe` [1, 6, 3, 2, 3, 6]
        isAbelian @g `shouldBe` True

  describe "DihedralGroup D_12" $ do
    it "has correct orders" $
      dihedralGroup 6 \ @g -> do
        checkGroup @g 12
        orders @g [
          (False, 0), (False, 1), (False, 2), (False, 3),
          (True, 0), (True, 1), (True, 2), (True, 3)
          ] `shouldBe` [1, 6, 3, 2, 2, 2, 2, 2]
        isAbelian @g `shouldBe` False

  describe "Permutation group S_4" $ do
    it "has correct orders" $
      permutationGroup 4 \ @g -> do
        checkGroup @g 24
        orders @g [[0..3], [1, 0, 3, 2], [2, 1, 3, 0], [1, 2, 3, 0]] `shouldBe` [1, 2, 3, 4]
        isAbelian @g `shouldBe` False

  describe "Units mod 5, 6, 9" $ do
    it "has correct orders" $ do
      unitsMod 5 \ @g -> do
        checkGroup @g 4
        orders @g [1, 2, 3, 4] `shouldBe` [1, 4, 4, 2]
        isAbelian @g `shouldBe` True
      unitsMod 6 \ @g -> do
        checkGroup @g 2
        orders @g [1, 5] `shouldBe` [1, 2]
        isAbelian @g `shouldBe` True
      unitsMod 9 \ @g -> do
        checkGroup @g 6
        orders @g [1, 2, 4, 5, 7, 8] `shouldBe` [1, 6, 3, 6, 3, 2]
        isAbelian @g `shouldBe` True

orders :: forall g p. Group g p => [p] -> [Int]
orders ps = orderElement . groupFrom @g <$> ps

checkGroup :: forall g p. Group g p => Int -> Expectation
checkGroup expectedOrder = do
  orderGroup @g `shouldBe` expectedOrder
  for_ (elementsList @g) $ \a -> do
    trace ("checking element " ++ show a) $ do
      a <> mempty @g `shouldBe` a
      mempty @g <> a `shouldBe` a
    for_ (elementsList @g) $ \b -> do
      for_ (elementsList @g) $ \c -> do
        a <> (b <> c) `shouldBe` (a <> b) <> c

{-
  add metaCyclics
-}