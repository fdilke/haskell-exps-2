{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Collapse lambdas" #-}

-- module LangFeature.Automata ( doNothing) where
module Automata where

doNothing :: Int -> Int
doNothing x = x

-- type Automaton1 s = (s, s -> s)
data Automaton1 s = Automaton1
  { initial :: s,
    update :: s -> s
  }

-- s0,s1,s2...
listStates1 :: Automaton1 s -> [s]
listStates1 automaton =
  iterate automaton.update automaton.initial

-- error "listStates undefined"

data Automaton2 state input = Automaton2
  { initial :: state,
    update :: input -> state -> state
  }

data Automaton3 state input = Automaton3
  { initial :: state,
    update :: input -> state -> state,
    acceptable :: state -> Bool
  }




-- foldr :: (a -> b -> b) -> b -> t a -> b
-- listStates2 :: forall state input. Automaton2 state input -> [input] -> [state]
-- listStates2 automaton inputs = listStates2sub inputs [automaton.initial]
--   where
--     listStates2sub :: [input] -> [state] -> [state]
--     listStates2sub [] states = states
--     listStates2sub (headInput:tailInputs) (headState:tailStates) = 
--         tailStates ++ (listStates2sub tailInputs [automaton.update headInput headState]) 


-- recursing over a list of inputs, each one used to do this calculation:
-- input, state -> new state
-- which is added to an (existing list of states) ++ 
--

listStates2' :: forall state input. Automaton2 state input -> [input] -> [state]
listStates2' automaton inputs =
    inner automaton.initial inputs
  where
    inner :: state -> [input] -> [state]
    inner state [] = [state]
    inner state (headInput : tailInputs) =
        state : inner (automaton.update headInput state) tailInputs

-- listStates2 automaton [] = [automaton.initial]
-- listStates2 automaton (head:tail) = automaton.initial ++ tail


finalState2 :: Automaton2 state input -> [input] -> state
-- finalState2 automaton inputs = foldr automaton.update automaton.initial inputs
finalState2 automaton inputs = foldl (flip automaton.update) automaton.initial inputs
listStates2 :: Automaton2 state input -> [input] -> [state]
listStates2 automaton inputs = scanl (flip automaton.update) automaton.initial inputs
-- not lazy enough!
-- listStates2 automaton inputs = scanr automaton.update automaton.initial inputs

-- listStates2 automaton inputs =
--     let
--         fn :: (input -> [state] -> [state])
--         fn = \i -> \states ->
--             let
--                 z = automaton.update i (last states)
--             in
--                 states ++ [z]
--         h = foldr fn [automaton.initial] inputs
--     in
--         h

-- in scanr fn automaton.initial inputs
-- iterate  automaton.update  automaton.initial
-- error "listStates undefined"


acceptInputs3 :: forall state input . Automaton3 state input -> [input] -> Bool
acceptInputs3 automaton inputs = automaton.acceptable $ foldl (flip automaton.update) automaton.initial inputs
