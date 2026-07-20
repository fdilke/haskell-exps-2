{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Group
where

import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)

class Group g where
  identity :: g
  inverse :: g -> g
  combine :: g -> g -> g

-- | The cyclic group Z/nZ, with the modulus @n@ carried at the type level.
newtype CyclicGroup (n :: Nat) = CyclicGroup Int deriving (Show, Eq)

instance KnownNat n => Group (CyclicGroup n) where
  identity = CyclicGroup 0
  inverse (CyclicGroup x) = CyclicGroup ((m - x) `mod` m)
    where
      m = fromIntegral (natVal (Proxy @n))
  combine (CyclicGroup x) (CyclicGroup y) = CyclicGroup ((x + y) `mod` m)
    where
      m = fromIntegral (natVal (Proxy @n))

-- | Reify a runtime order @n@ into a type-level cyclic group and hand the
-- resulting 'Group' instance to a polymorphic continuation.
cyclicGroup :: Int -> (forall g. Group g => Proxy g -> h) -> h
cyclicGroup n f =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy m) -> f (Proxy @(CyclicGroup m))
