{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeAbstractions #-}
{- HLINT ignore "Redundant negate" -}
{- HLINT ignore "Use -" -}

module Test.Algebra.FieldSpec where

import Data.Either (isRight)
import Graph
import Test.Hspec
import Algebra.Field (FieldTable(..), Field(..), mkFieldTable, withField)
import Algebra.ConwayTable (conwayTable)
import Debug.Trace (trace)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Functor ((<&>))
import Data.Foldable (for_)

spec :: Spec
spec = do
  describe "Finite field algebra" $ do
    it "can construct field tables for GF(25)" $ do
      let ft :: FieldTable = mkFieldTable 5 2 [2,4]
      ft.addTable Map.! (7, 23) `shouldBe` 0
      ft.addTable Map.! (23, 23) `shouldBe` 16
      ft.negTable Map.! 23 `shouldBe` 7
      ft.mulTable Map.! (2, 3) `shouldBe` 1
      ft.mulTable Map.! (2, 5) `shouldBe` 10
      ft.mulTable Map.! (5, 5) `shouldBe` 8
      ft.mulTable Map.! (10, 15) `shouldBe` 8
      ft.mulTable Map.! (6, 6) `shouldBe` 19
      ft.invTable Map.! 7 `shouldBe` 16
    it "can use GF(25) in withField" $ do
      withField 25 \ @f -> do
        let (+++) = fieldAdd @f
        let (***) = fieldMul @f
        let negg = fieldNeg @f
        let inv = fieldInv @f
        7 +++ 23 `shouldBe` 0
        23 +++ 23 `shouldBe` 16
        negg 23 `shouldBe` 7
        2 *** 3 `shouldBe` 1
        2 *** 5 `shouldBe` 10
        5 *** 5 `shouldBe` 8
        10 *** 15 `shouldBe` 8
        6 *** 6 `shouldBe` 19
        inv 7 `shouldBe` 16
        
    it "can do field calculations in scope" $ do
      withField 8 \ @f -> do
        checkField @f 8

checkField :: forall f. Field f => Int -> Expectation
checkField pn = do
  orderField @f `shouldBe` pn
  let elements :: [f] = fieldElements @f
      zero = 0 :: f
      one = 1 :: f
      xx :: Int = 3
      yy :: f = fromInteger (toInteger xx )
  for_ elements $ \a -> do
    a + zero `shouldBe` a
    a + negate a `shouldBe` zero
    a - a `shouldBe` zero
    negate (negate a) `shouldBe` a
    a * one `shouldBe` a
    if a /= zero then do
      a * recip a `shouldBe` one
      recip (recip a) `shouldBe` a
    else do
      pure ()
    for_ elements $ \b -> do
      a + b `shouldBe` b + a
      a * b `shouldBe` b * a
      for_ elements $ \c -> do
        a + (b + c) `shouldBe` (a + b) + c
        a * (b * c) `shouldBe` (a * b) * c
        a * (b + c) `shouldBe` (a * b) + (a * c)
    -- TODO check more laws




