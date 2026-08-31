{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeAbstractions #-}

module Test.Algebra.FieldSpec where

import Data.Either (isRight)
import Graph
import Test.Hspec
import Algebra.Field (FieldTable(..), fieldTable, withField)
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
      let ft :: FieldTable = fieldTable 5 2 [2,4]
      ft.addTable Map.! (7, 23) `shouldBe` 0
      ft.addTable Map.! (23, 23) `shouldBe` 16
      ft.negTable Map.! 23 `shouldBe` 7
      ft.mulTable Map.! (2, 3) `shouldBe` 1
      ft.mulTable Map.! (2, 5) `shouldBe` 10
      ft.mulTable Map.! (5, 5) `shouldBe` 8
      ft.mulTable Map.! (10, 15) `shouldBe` 8
      ft.mulTable Map.! (6, 6) `shouldBe` 19
      ft.invTable Map.! 7 `shouldBe` 16
    it "are field tables sensible? (0)" $ do
      let h = take 3 conwayTable
          k = trace ("the tables:" <> show h) 0
      k `shouldBe` (0 :: Int)
    -- it "are field tables2 sensible?" $ do
    --   let h = take 3 (Map.toList conwayTable2) <&> \(_, f) -> f ()
    --       k = trace ("the tables:" <> show h) 0
    --   k `shouldBe` (0 :: Int)
    -- it "are field tables2 sensible?" $ do
    --   let (p, h) = head (Map.toList conwayTable2)
    --       k = trace ("the tables:" <> show (h ())) 0
    --   -- let h = (take 3 (Map.toList conwayTable2)) <&> \(_, f) -> f ()
    --   --     k = trace ("the tables:" <> show h) 0
    --   k `shouldBe` (0 :: Int)
    it "can do field calculations in scope" $ do
      withField 8 \ @f -> do
        checkField @f 8

checkField :: forall f. (Num f, Fractional f, Show f, Eq f) => Int -> Expectation
checkField pn = do
  -- orderGroup @g `shouldBe` expectedOrder
  let elements :: [f] = [0..(pn-1)] <&> (fromInteger . toInteger)
      zero = 0 :: f
      one = 1 :: f
      xx :: Int = 3
      yy :: f = fromInteger (toInteger xx )
  for_ elements $ \a -> do
    a + zero `shouldBe` a
    zero + a `shouldBe` a
    a * one `shouldBe` a
    one * a `shouldBe` a
    for_ elements $ \b -> do
      for_ elements $ \c -> do
        a + (b + c) `shouldBe` (a + b) + c
    -- TODO check more laws




  