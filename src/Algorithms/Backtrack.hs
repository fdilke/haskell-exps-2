{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Algorithms.Backtrack
where

transformEithers :: forall a b . [Either a b] -> ([a], [b])
transformEithers = foldr go ([], [])
  where
    go :: Either a b -> ([a], [b]) -> ([a], [b])
    go (Left a) (as, bs) = (a:as, bs)
    go (Right b) (as, bs) = (as, b:bs)

solve :: forall a b. a -> (a -> [Either a b]) -> [b]
solve start next = []