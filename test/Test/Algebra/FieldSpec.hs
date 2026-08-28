{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Algebra.FieldSpec where

import Data.Either (isRight)
import Graph
import Test.Hspec
import Algebra.Field (FieldTable(..), FieldTableOps(..))

spec :: Spec
spec = do
  describe "Finite field algebra" $ do
    it "can construct field tables for GF(25)" $ do
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
      addPolys ft [2, 1] [3, 4] `shouldBe` [0, 0]
      addPolys ft [3, 4] [3, 4] `shouldBe` [1, 3]
      negPoly ft [2, 1] `shouldBe` [3, 4]
      negPoly ft [3, 4] `shouldBe` [2, 1]
      scalarMult ft 2 [3, 4] `shouldBe` [1, 3]
      shift ft [3, 4] `shouldBe` [2, 2]
      mulPolys ft [2, 0] [3, 0] `shouldBe` [1, 0]
      mulPolys ft [2, 0] [0, 1] `shouldBe` [0, 2]
      mulPolys ft [0, 1] [0, 1] `shouldBe` [3, 1]
      mulPolys ft [0, 2] [0, 3] `shouldBe` [3, 1]
      mulPolys ft [1, 1] [1, 1] `shouldBe` [4, 3]
      mulPolys ft [1, 2] [3, 4] `shouldBe` [2, 3]
      mulPolys ft [2, 1] [4, 3] `shouldBe` [2, 3]
      mulPolys ft [2, 1] (inversePoly ft [2, 1]) `shouldBe` [1, 0]
      inversePoly ft [2, 1] `shouldBe` [1, 3]

  