{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Newton(mySqrt) where

import Data.List(find)

-- newt :: Fractional a => a -> a -> a
-- newt q x = (x + q/x)/2

closeEnough :: Ord a => Fractional a => (a -> a) -> a -> a -> Bool
closeEnough f tolerance est = abs (f est) < tolerance

standardTolerance :: Fractional a => a
standardTolerance = 1e-6

nextIterate :: Ord a => Fractional a => (a -> a) -> (a -> a) -> a -> a
nextIterate f f' x =
  x - f x / f' x

myNewton :: forall a. Ord a => Fractional a => (a -> a) -> (a -> a) -> a -> a
myNewton f f' start =
  case find (closeEnough f standardTolerance) (iterate (nextIterate f f') start) of
    Just root -> root
    Nothing -> -1

mySqrt :: forall a. Ord a => Fractional a => a -> a
mySqrt q =
  myNewton f f' (q/2)
  where
    f :: a -> a
    f x = x*x - q
    f' :: a -> a
    f' x = 2*x
