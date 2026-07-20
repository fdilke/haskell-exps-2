{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.GroupSpec where

import Data.Proxy (Proxy (..))
import Group
import Test.Hspec

spec :: Spec
spec = do
  describe "CyclicGroup (Z/5Z)" $ do
    let e = identity :: CyclicGroup 5
    it "has an identity that is a two-sided unit" $ do
      combine e (CyclicGroup 3) `shouldBe` (CyclicGroup 3 :: CyclicGroup 5)
      combine (CyclicGroup 3) e `shouldBe` (CyclicGroup 3 :: CyclicGroup 5)

    it "wraps around modulo n when combining" $
      combine (CyclicGroup 3) (CyclicGroup 4) `shouldBe` (CyclicGroup 2 :: CyclicGroup 5)

    it "computes inverses that cancel to the identity" $ do
      combine (CyclicGroup 3) (inverse (CyclicGroup 3)) `shouldBe` e
      combine (inverse (CyclicGroup 2)) (CyclicGroup 2) `shouldBe` e

    it "inverts the identity to itself" $
      inverse e `shouldBe` e

    it "inverts each element to n - x" $
      inverse (CyclicGroup 1) `shouldBe` (CyclicGroup 4 :: CyclicGroup 5)

  describe "cyclicGroup" $
    it "reifies a runtime order and runs the continuation over its Group" $
      cyclicGroup 7 (\(Proxy :: Proxy g) -> combine (identity @g) (identity @g) `seq` (42 :: Int))
        `shouldBe` 42
