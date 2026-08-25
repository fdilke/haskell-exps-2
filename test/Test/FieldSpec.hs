{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.FieldSpec where

import Data.Either (isRight)
import Graph
import Test.Hspec
import Field (FieldTable(..), FieldTableOps(..))

spec :: Spec
spec = do
  describe "Finite field algebra" $ do
    it "can construct field tables" $ do
      let ft = FieldTable 5 2 [2,4]
      ft.prime `shouldBe` 5
      ft.power `shouldBe` 2
      ft.primitive `shouldBe` [2,4]
      intToPoly ft 0 `shouldBe` [0, 0]
      intToPoly ft 2 `shouldBe` [2, 0]
      intToPoly ft 7 `shouldBe` [2, 1]
      intToPoly ft 23 `shouldBe` [3, 4]
      polyToInt ft [0, 0] `shouldBe` 0
      polyToInt ft [2, 0] `shouldBe` 2
      polyToInt ft [2, 1] `shouldBe` 7
      polyToInt ft [3, 4] `shouldBe` 23

  