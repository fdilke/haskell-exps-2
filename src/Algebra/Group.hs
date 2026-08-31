{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Algebra.Group (
  Group(..),
  Switch(..),
  cyclicGroup,
  dihedralGroup,
  permutationGroup,
  unitsMod,
  metacyclic,
  orderElement,
  orderGroup,
  isAbelian,
) where

import Data.Bits (xor)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Singletons.Base.TH
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), someNatVal)
import Data.List
import Data.Maybe (listToMaybe)
import Control.Exception (throw, AssertionFailed (AssertionFailed))
import Utility.Utility

class Switch g p where
  switchFrom :: p -> g
  switchTo :: g -> p

class (Eq g, Ord g, Show g, Monoid g) => Group g where
  elements :: Set g
  inverse :: g -> g
  elementsList :: [g]
  elementsList = Set.toList elements

instance (KnownNat n) => Semigroup (Power n) where
  (<>) (Power x) (Power y) = Power ((x + y) `mod` nat @n)

instance (KnownNat n) => Monoid (Power n) where
  mempty = Power 0

-- | The cyclic group Z/nZ, with the nat @n@ carried at the type level.
newtype Power (n :: Nat) = Power Int deriving (Show, Eq, Ord)

instance (KnownNat n) => Group (Power n) where
  elements = Set.fromList $ Power <$> [0 .. nat @n - 1]
  inverse (Power x) = Power ((nat @n - x) `mod` nat @n)

instance (KnownNat n) => Switch (Power n) Int where
  switchFrom = Power
  switchTo (Power x) = x

{- | Reify a runtime order @n@ into a type-level cyclic group and hand the
resulting 'Group' instance to a polymorphic continuation.
-}
cyclicGroup :: Int -> (forall g. (Group g, Switch g Int) => h) -> h
cyclicGroup n f =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy m) -> f @(Power m)

-- TODO fix
-- generateSubgroup :: forall g. Group g => Set g -> Set g
-- generateSubgroup set = set

orderGroup :: forall g. (Group g) => Int
orderGroup = Set.size (elements @g)

orderElement :: forall g. (Group g) => g -> Int
-- orderElement x = length $ generateSubgroup $ Set.singleton x
orderElement x = test 1 x
 where
  test n y
    | y == mempty = n
    | otherwise = test (n + 1) (x <> y)

commutes :: forall g. (Group g) => g -> g -> Bool
commutes x y = x <> y == y <> x

isAbelian :: forall g. (Group g) => Bool
isAbelian = all (uncurry commutes) pairs
 where
  pairs :: [(g, g)]
  pairs = [(x, y) | x <- elementsList, y <- elementsList]

newtype DihedralElement (n :: Nat) = DihedralElement (Bool, Int) deriving (Show, Eq, Ord)

instance (KnownNat n) => Semigroup (DihedralElement n) where
  (<>) (DihedralElement (b, x)) (DihedralElement (c, y)) =
    DihedralElement (xor b c, (if c then y - x + nat @n else y + x) `mod` nat @n)

instance (KnownNat n) => Monoid (DihedralElement n) where
  mempty = DihedralElement (False, 0)

instance (KnownNat n) => Group (DihedralElement n) where
  elements = Set.fromList $ DihedralElement <$> vals2
   where
    vals = [0 .. nat @n - 1]
    vals2 = [(b, x) | b <- [True, False], x <- vals]
  inverse e@(DihedralElement (b, x)) =
    if b
      then e
      else
        DihedralElement (False, (nat @n - x) `mod` nat @n)

instance (KnownNat n) => Switch (DihedralElement n) (Bool, Int) where
  switchFrom = DihedralElement
  switchTo (DihedralElement bx) = bx

dihedralGroup :: Int -> (forall g. (Group g, Switch g (Bool, Int)) => h) -> h
dihedralGroup n f =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy m) -> f @(DihedralElement m)

newtype Permutation (n :: Nat) = Permutation [Int] deriving (Show, Eq, Ord)

instance (KnownNat n) => Semigroup (Permutation n) where
  (<>) (Permutation p) (Permutation q) =
    Permutation ((p !!) <$> q)

instance (KnownNat n) => Monoid (Permutation n) where
  mempty = Permutation [0 .. nat @n - 1]

instance (KnownNat n) => Group (Permutation n) where
  elements = Set.fromList $ Permutation <$> vals
   where
    vals = permutations [0 .. nat @n - 1]
  inverse (Permutation p) =
    let n = length p
        q = replicate n 0
     in Permutation $ foldr (\(i, j) acc -> take j acc ++ [i] ++ drop (j + 1) acc) q (zip [0 ..] p)

instance (KnownNat n) => Switch (Permutation n) [Int] where
  switchFrom = Permutation
  switchTo (Permutation p) = p

permutationGroup :: Int -> (forall g. (Group g, Switch g [Int]) => h) -> h
permutationGroup n f =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy m) -> f @(Permutation m)

newtype UnitMod (n :: Nat) = UnitMod Int deriving (Show, Eq, Ord)

instance (KnownNat n) => Semigroup (UnitMod n) where
  (<>) (UnitMod a) (UnitMod b) =
    UnitMod $ (a * b) `mod` (nat @n)

instance (KnownNat n) => Monoid (UnitMod n) where
  mempty = UnitMod 1

instance (KnownNat n) => Group (UnitMod n) where
  elements = Set.fromList $ UnitMod <$> vals
   where
    theN = nat @n
    vals = [ a | a <- [1 .. theN - 1], gcd a theN == 1 ]
  inverse (UnitMod a) =
    case listToMaybe [ b | b <- [1 .. nat @n - 1], (a * b) `mod` nat @n== 1 ] of
      Just c -> UnitMod c
      _ -> UnitMod 1

instance (KnownNat n) => Switch (UnitMod n) Int where
  switchFrom = UnitMod
  switchTo (UnitMod a) = a

unitsMod :: Int -> (forall g. (Group g, Switch g Int) => h) -> h
unitsMod n f =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy m) -> f @(UnitMod m)

powMod :: Int -> Int -> Int -> Int
powMod _ 0 _ = 1
powMod base expt m
  | even expt  = let half = powMod base (expt `div` 2) m
                in (half * half) `mod` m
  | otherwise = (base `mod` m * powMod base (expt - 1) m) `mod` m

$( singletons
     [d|
       data MetacyclicData = MetacyclicData { theP :: Nat, theQ :: Nat, theR :: Nat }
         deriving (Show, Eq)
       |]
 )

data MetacyclicElement (f :: MetacyclicData) = MetacyclicElement {
  bExp :: Int,
  aExp :: Int  -- representing b^bExp * a^aExp in G = <a, b | a^p = 1, b^q = a^r, a^b = a^r>
} deriving (Show, Eq, Ord)

{- | The type-level 'MetacyclicData' demoted once, to plain 'Int's, so the
instances below can just say @m.p@ instead of unpacking a singleton each time.
-}
data MetacyclicParams = MetacyclicParams {p :: Int, q :: Int, r :: Int}

metaParams :: forall (f :: MetacyclicData). (SingI f) => MetacyclicParams
metaParams = case fromSing (sing @f) of
  MetacyclicData pp qq rr ->
    MetacyclicParams (fromIntegral pp) (fromIntegral qq) (fromIntegral rr)

instance (SingI f) => Semigroup (MetacyclicElement f) where
  (<>) (MetacyclicElement x y) (MetacyclicElement z w) =
    MetacyclicElement {
      bExp = (x + z) `mod` m.q,
      aExp = (y * powMod m.r z m.p + w) `mod` m.p
    }
   where m = metaParams @f

instance (SingI f) => Monoid (MetacyclicElement f) where
  mempty = MetacyclicElement { bExp = 0, aExp = 0 }

instance (SingI f) => Group (MetacyclicElement f) where
  elements = Set.fromList do
      x <- [0 .. m.q - 1]
      y <- [0 .. m.p - 1]
      pure MetacyclicElement { bExp = x, aExp = y }
   where m = metaParams @f
  inverse (MetacyclicElement x y) =
    MetacyclicElement {
      bExp = m.q - x,
      aExp = m.q - (y * powMod m.r (m.q - x) m.p) `mod` m.p
    }
   where m = metaParams @f

instance (SingI f) => Switch (MetacyclicElement f) (Int, Int) where
  switchFrom (b, a) = MetacyclicElement { bExp = b, aExp = a }
  switchTo (MetacyclicElement b a) = (b, a)

metacyclic :: Int -> Int -> (forall g. (Group g, Switch g (Int, Int)) => h) -> h
metacyclic p q block =
  let r :: Int = unitsMod p \ @g ->
        case find (\x -> q == orderElement @g x) (elementsList @g) of
                  Just x -> switchTo @g x
                  Nothing -> throw $ AssertionFailed "no suitable exponent found"
  in case (
    someNatVal (fromIntegral p), 
    someNatVal (fromIntegral q), 
    someNatVal (fromIntegral r)
    ) of ( 
          SomeNat (_ :: Proxy mp), 
          SomeNat (_ :: Proxy mq), 
          SomeNat (_ :: Proxy mr)
          ) ->
          block @(MetacyclicElement ('MetacyclicData mp mq mr))
