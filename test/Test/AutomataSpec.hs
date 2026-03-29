{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.AutomataSpec where

import Automata
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "function successfully does nothing" $ do
    it "mimic (+1) function" $
      doNothing 4 `shouldBe` 4
  describe "automaton1 can list its output states" $
    let automaton1 :: Automaton1 S
        automaton1 = Automaton1 {initial = A, update = \_ -> B}
     in do
          it "does stuff" $
            take 2 (listStates1 automaton1) `shouldBe` [A, B]
  describe "automaton2 can process a list of inputs and output the resulting states" $
    let automaton2 :: Automaton2 Int Int
        automaton2 = Automaton2 {initial = 0, update = \s n -> s + n}
     in do
          it "does stuff" $ do
            take 5 (listStates2 automaton2 ([4, 7, 12] ++ repeat 1)) `shouldBe` [0, 4, 11, 23, 24]
            listStates2 automaton2 [4, 7, 12] `shouldBe` [0, 4, 11, 23]
  describe "automaton3 can process a list of inputs and output the resulting states" $ do
    let automaton3 :: Automaton3 Parity ()
        automaton3 = Automaton3 {initial = Even, update = \_ s -> flipP s , acceptable = (== Odd)}
     in do
          prop "Automaton3 tests" $ \inputs -> 
            acceptInputs3 automaton3 inputs == (length inputs `mod` 2 == 1)
          it "uses correct acceptance criteria" $ do
            acceptInputs3 automaton3 [(),(),()] `shouldBe` True
            acceptInputs3 automaton3 [(),()] `shouldBe` False
            acceptInputs3 automaton3 [()] `shouldBe` True
            acceptInputs3 automaton3 [] `shouldBe` False
            -- take 5 (listStates3 automaton3 ([4, 7, 12] ++ repeat 1)) `shouldBe` [0, 4, 11, 23, 24]
            -- listStates3 automaton2 [4, 7, 12] `shouldBe` [0, 4, 11, 23]


data S = A | B | C deriving (Eq, Show)
data Parity = Even | Odd deriving (Eq, Show)

flipP :: Parity -> Parity
flipP Even = Odd
flipP Odd = Even

