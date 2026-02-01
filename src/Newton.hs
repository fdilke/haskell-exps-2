{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Newton(mySqrt) where

import Data.List(find)

-- newt :: Fractional a => a -> a -> a
-- newt q x = (x + q/x)/2

closeEnough :: Ord a => Fractional a => a -> a -> a -> Bool
closeEnough q tolerance est = abs (est * est - q) < tolerance

standardTolerance :: Fractional a => a
standardTolerance = 1e-6

myNewton :: Ord a => Fractional a => (a -> a) -> (a -> a) -> a -> a
myNewton f f' x =
  x - f x / f' x

mySqrt :: forall a. Ord a => Fractional a => a -> a
mySqrt q =
   case find (closeEnough q standardTolerance) (iterate (myNewton f f') (q/2)) of
     Just root -> root
     Nothing -> -1
    where
      f :: a -> a
      f x = x*x - q
      f' :: a -> a
      f' x = 2*x
