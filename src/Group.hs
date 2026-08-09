{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Group(cyclicGroup, Group(..), orderElement, isAbelian) where

import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Singletons.Base.TH

class (Eq g, Ord g,Show g) => Group g p | g -> p where
  elements :: Set g
  groupFrom :: p -> g
  groupTo :: g -> p
  identity :: g
  inverse :: g -> g
  combine :: g -> g -> g
  elementsList :: [g]
  elementsList = Set.toList elements

-- | The cyclic group Z/nZ, with the modulus @n@ carried at the type level.
newtype CyclicGroup (n :: Nat) = CyclicGroup Int deriving (Show, Eq, Ord)

instance KnownNat n => Group (CyclicGroup n) Int where
  elements = Set.fromList $ CyclicGroup <$> [0 .. fromIntegral (natVal (Proxy @n)) - 1]
  groupFrom = CyclicGroup
  groupTo (CyclicGroup x) = x
  identity = CyclicGroup 0
  inverse (CyclicGroup x) = CyclicGroup ((m - x) `mod` m)
    where
      m = fromIntegral (natVal (Proxy @n))
  combine (CyclicGroup x) (CyclicGroup y) = CyclicGroup ((x + y) `mod` m)
    where
      m = fromIntegral (natVal (Proxy @n))

-- | Reify a runtime order @n@ into a type-level cyclic group and hand the
-- resulting 'Group' instance to a polymorphic continuation.
cyclicGroup :: Int -> (forall g p. Group g Int => Proxy (g, Int) -> h) -> h
cyclicGroup n f =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy m) -> f (Proxy @(CyclicGroup m, Int))

-- TODO fix
-- generateSubgroup :: forall g. Group g => Set g -> Set g
-- generateSubgroup set = set

orderElement :: forall g p. Group g p => g -> Int
-- orderElement x = length $ generateSubgroup $ Set.singleton x
orderElement x = test 1 x where
  test n y
    | y == identity = n
    | otherwise = test (n + 1) (combine x y)

-- isAbelian :: forall g p. Group g p => Proxy g -> Bool
-- isAbelian _ = all (\(x, y) -> combine x y == combine y x) pairs
--   where
--     pairs :: [(g, g)]
--     pairs = [(x, y) | x <- elements, y <- elements]

commutes :: forall g p. Group g p => g -> g -> Bool
commutes x y = combine x y == combine y x

isAbelian :: forall g p. Group g p =>  Bool
isAbelian = all (uncurry commutes) pairs
  where
    pairs :: [(g, g)]
    pairs = [(x, y) | x <- elementsList, y <- elementsList]
    
$(singletons [d|
  data Foo = Foo { fooName :: String, fooCount :: Nat }
    deriving (Show, Eq)
  |])
  
newtype Widget (f :: Foo) = Widget { widgetId :: Int }

label :: forall f. SingI f => Widget f -> String
label _ = case fromSing (sing @f) of
  Foo nm cnt -> nm ++ " x" ++ show cnt

myWidget :: Widget ('Foo (FromString "widget") 3)
myWidget = Widget { widgetId = 42 }

