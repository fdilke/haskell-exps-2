{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.GroupSpec where

import Data.Proxy (Proxy (..))
import Group
import Test.Hspec

spec :: Spec
spec = do
  describe "CyclicGroup (Z/5Z)" $ do
    it "has correct orders" $
      cyclicGroup 6 (\(Proxy :: Proxy (g, Int)) -> do
        -- combine (identity @g) (identity @g) `seq` (42 :: Int)
        --   `shouldBe` 42
        combine (identity @g) (identity @g) `shouldBe` (identity @g)
        orderElement (groupFrom @g 0) `shouldBe` 1
        orderElement (groupFrom @g 1) `shouldBe` 6
        orderElement (groupFrom @g 2) `shouldBe` 3
        orderElement (groupFrom @g 3) `shouldBe` 2
        orderElement (groupFrom @g 4) `shouldBe` 3
        orderElement (groupFrom @g 5) `shouldBe` 6
        isAbelian @g `shouldBe` True
      )
      
  describe "cyclicGroup" $
    it "reifies a runtime order and runs the continuation over its Group" $
      cyclicGroup 7 (\(Proxy :: Proxy (g, Int)) -> combine (identity @g) (identity @g) `seq` (42 :: Int))
        `shouldBe` 42
