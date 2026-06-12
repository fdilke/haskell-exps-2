{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Algorithms.BacktrackSpec where

import Algorithms.Backtrack
import Test.Hspec
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.IORef (newIORef, IORef, readIORef)
import Data.List (sort)

spec :: Spec
spec = do
  describe "Backtracking" $ do
    it "transforms a list of Eithers" $
      transformEithers [Left 1, Right "a", Left 2, Left 3, Right "b"]
        `shouldBe` (([1, 2, 3], ["a", "b"]) :: ([Int], [String]))
    it "gives 0 solutions when there are none" $
      solve @Bool @Int True (\_ -> []) `shouldBe` []
    it "gives 1 solutions when there is exactly one" $
      solve @Bool @Int True (\_ -> [Right 1]) `shouldBe` [1]
    it "successfully increments a value to 5" $
      (solve @Int @Bool
        0
        \i ->
            if i == 5
              then [Right True]
              else [Left (i + 1)])
        `shouldBe` [True]

    it "finds a solution in a branching search" $
      let seqValues :: [Bool]
          seqValues = [True, False]
          -- state is [Bool], result is [Bool]
          solutions = solve @[Bool] @[Bool] [] (\prefix ->
            if length prefix == 3
              then [Right prefix]
              else concatMap (\v -> [Left (prefix ++ [v])]) seqValues)
       in sort solutions
            `shouldBe` sort [ [False, False, False]
                       , [False, False, True]
                       , [False, True, False]
                       , [False, True, True]
                       , [True, False, False]
                       , [True, False, True]
                       , [True, True, False]
                       , [True, True, True]
                       ]
  -- test("find a solution in a branching search"):
  --   val seqValues: Iterable[Boolean] =
  --     Iterable(true, false)
  --   val explorations: AtomicReference[Seq[Seq[Boolean]]] =
  --     AtomicReference[Seq[Seq[Boolean]]]:
  --       Seq.empty
  --   solver(Seq.empty[Boolean]): prefix =>
  --       explorations.set:
  --         explorations.get() :+ prefix
  --       if (prefix.length == 3)
  --         Iterable(Right(prefix))
  --       else
  --         seqValues.map: v =>
  --           Left(prefix :+ v)
  --   .toSet is Set(
  --     Seq(false, false, false),
  --     Seq(false, false, true),
  --     Seq(false, true, false),
  --     Seq(false, true, true),
  --     Seq(true, false, false),
  --     Seq(true, false, true),
  --     Seq(true, true, false),
  --     Seq(true, true, true)
  --   )
  --   explorations.get().size is 15

--  test("successfully increments a value to 5"):
--     solver[Int, Boolean](0): i =>
--       if (i == 5)
--         Iterable(Right(true))
--       else
--         Iterable(Left(i + 1))
--     .headOption is Some(true)
