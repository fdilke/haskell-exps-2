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
      cyclicGroup 6 (\ @g -> do
        -- combine (identity @g) (identity @g) `seq` (42 :: Int)
        --   `shouldBe` 42
        checkGroup @g 6
        (mempty @g) <> (mempty @g) `shouldBe` (mempty @g)
        orderElement (groupFrom @g 0) `shouldBe` 1
        orderElement (groupFrom @g 1) `shouldBe` 6
        orderElement (groupFrom @g 2) `shouldBe` 3
        orderElement (groupFrom @g 3) `shouldBe` 2
        orderElement (groupFrom @g 4) `shouldBe` 3
        orderElement (groupFrom @g 5) `shouldBe` 6
        isAbelian @g `shouldBe` True
      )

  describe "DihedralGroup D_12" $ do
    it "has correct orders" $
      dihedralGroup 6 (\ @g -> do
        checkGroup @g 12
        (mempty @g) <> (mempty @g) `shouldBe` (mempty @g)
        orderGroup @g `shouldBe` 12
        orderElement (groupFrom @g (False, 0)) `shouldBe` 1
        orderElement (groupFrom @g (False, 1)) `shouldBe` 6
        orderElement (groupFrom @g (False, 2)) `shouldBe` 3
        orderElement (groupFrom @g (False, 3)) `shouldBe` 2
        orderElement (groupFrom @g (True, 0)) `shouldBe` 2
        orderElement (groupFrom @g (True, 1)) `shouldBe` 2
        orderElement (groupFrom @g (True, 2)) `shouldBe` 2
        orderElement (groupFrom @g (True, 3)) `shouldBe` 2
        isAbelian @g `shouldBe` False
      )

  describe "Permutation group S_4" $ do
    it "has correct orders" $
      permutationGroup 4 (\ @g -> do
        checkGroup @g 24
        orderElement . groupFrom @g <$> [[0..3], [1, 0, 3, 2], [2, 1, 3, 0],[1, 2, 3, 0]] `shouldBe` [1, 2, 3, 4]
        isAbelian @g `shouldBe` False
      )

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
  groups now extend Monoid,
-}