{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Algorithms.Backtrack (solve, transformEithers, solveM)
where

import Data.Foldable (foldrM)

transformEithers :: forall a b. [Either a b] -> ([a], [b])
transformEithers = foldr combineEither ([], [])
 where
  combineEither :: Either a b -> ([a], [b]) -> ([a], [b])
  combineEither (Left a) (as, bs) = (a : as, bs)
  combineEither (Right b) (as, bs) = (as, b : bs)

solve :: forall a b. a -> (a -> [Either a b]) -> [b]
solve start next = doIt [start]
 where
  doIt :: [a] -> [b]
  doIt [] = []
  doIt (x : as0) = bs <> doIt (as0 <> as)
   where
    (as, bs) = transformEithers (next x)

transformEithersM :: forall a b m. (Monad m) => [Either a b] -> m ([a], [b])
transformEithersM = foldrM combineEitherM ([], [])
 where
  combineEitherM :: Either a b -> ([a], [b]) -> m ([a], [b])
  combineEitherM ab (as, bs) = case ab of
    Left a -> pure (a : as, bs)
    Right b -> pure (as, b : bs)

solveM :: forall a b m. (Monad m) => a -> (a -> m [Either a b]) -> m [b]
solveM start next = do
  let doIt :: [a] -> m [b]
      doIt [] = pure []
      doIt (x : as0) = do
        nx <- next x
        (as, bs) <- transformEithersM nx
        (bs <>) <$> doIt (as0 <> as)
  doIt [start]
