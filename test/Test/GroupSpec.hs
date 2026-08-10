{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}

module Test.GroupSpec where

import Group
import Test.Hspec

spec :: Spec
spec = do
  describe "CyclicGroup (Z/5Z)" $ do
    it "has correct orders" $
      cyclicGroup 6 (\ @g -> do
        -- combine (identity @g) (identity @g) `seq` (42 :: Int)
        --   `shouldBe` 42
        combine (identity @g) (identity @g) `shouldBe` (identity @g)
        orderGroup @g `shouldBe` 6
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
        -- combine (identity @g) (identity @g) `seq` (42 :: Int)
        --   `shouldBe` 42
        combine (identity @g) (identity @g) `shouldBe` (identity @g)
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
