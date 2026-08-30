{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Utility.Utility (nat, associativeOpPower, tabulate, tabulate2)
where

import GHC.TypeNats (KnownNat, natVal)
import Data.Singletons.Base.TH
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Functor ((<&>))

nat :: forall n. (KnownNat n) => Int
nat = fromIntegral (natVal (Proxy @n))

associativeOpPower :: forall a. (a -> a -> a) -> a -> Int -> a
associativeOpPower op x n
    | n == 1 = x
    | n `mod` 2 == 1 = op x (associativeOpPower op x (n -1))
    | otherwise = op y y where y = associativeOpPower op x (n `div` 2)

tabulate :: forall a b. Ord a => [a] -> (a -> b) -> Map a b
tabulate as f =
    Map.fromList $ as <&> \x ->
        (x, f x)

tabulate2 :: forall a b c. (Ord a, Ord b) => [a] -> [b] -> (a -> b -> c) -> Map (a, b) c
tabulate2 as bs f2 =
    Map.fromList $ do
        xA <- as
        xB <- bs
        pure ((xA, xB), f2 xA xB)
