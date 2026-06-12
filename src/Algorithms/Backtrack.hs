{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Algorithms.Backtrack
where

transformEithers :: forall a b . [Either a b] -> ([a], [b])
transformEithers = foldr combineEither ([], [])
  where
    combineEither :: Either a b -> ([a], [b]) -> ([a], [b])
    combineEither (Left a) (as, bs) = (a:as, bs)
    combineEither (Right b) (as, bs) = (as, b:bs)

solve :: forall a b. a -> (a -> [Either a b]) -> [b]
solve start next = doIt [start]
  where
    doIt :: [a] -> [b]
    doIt [] = []
    doIt (x : as0) = bs <> doIt ( as0 <> as )
      where
        (as, bs) = transformEithers (next x)
