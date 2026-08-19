{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Group (
  Group (..),
  cyclicGroup,
  dihedralGroup,
  permutationGroup,
  unitsMod,
  orderElement,
  orderGroup,
  isAbelian,
) where

import Data.Bits (xor)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Singletons.Base.TH
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)
import Data.List
import Data.Maybe (listToMaybe)

class (Eq g, Ord g, Show g, Monoid g) => Group g p | g -> p where
  elements :: Set g
  groupFrom :: p -> g
  groupTo :: g -> p
  inverse :: g -> g
  elementsList :: [g]
  elementsList = Set.toList elements

nat :: forall n. (KnownNat n) => Int
nat = fromIntegral (natVal (Proxy @n))

instance (KnownNat n) => Semigroup (Power n) where
  (<>) (Power x) (Power y) = Power ((x + y) `mod` nat @n)

instance (KnownNat n) => Monoid (Power n) where
  mempty = Power 0
  
-- | The cyclic group Z/nZ, with the nat @n@ carried at the type level.
newtype Power (n :: Nat) = Power Int deriving (Show, Eq, Ord)

instance (KnownNat n) => Group (Power n) Int where
  elements = Set.fromList $ Power <$> [0 .. nat @n - 1]
  groupFrom = Power
  groupTo (Power x) = x
  inverse (Power x) = Power ((nat @n - x) `mod` nat @n)

{- | Reify a runtime order @n@ into a type-level cyclic group and hand the
resulting 'Group' instance to a polymorphic continuation.
-}
cyclicGroup :: Int -> (forall g. (Group g Int) => h) -> h
cyclicGroup n f =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy m) -> f @(Power m)

-- TODO fix
-- generateSubgroup :: forall g. Group g => Set g -> Set g
-- generateSubgroup set = set

orderGroup :: forall g p. (Group g p {- Proxy g -> -}) => Int
orderGroup = Set.size (elements @g @p)

orderElement :: forall g p. (Group g p) => g -> Int
-- orderElement x = length $ generateSubgroup $ Set.singleton x
orderElement x = test 1 x
 where
  test n y
    | y == mempty = n
    | otherwise = test (n + 1) (x <> y)

commutes :: forall g p. (Group g p) => g -> g -> Bool
commutes x y = x <> y == y <> x

isAbelian :: forall g p. (Group g p) => Bool
isAbelian = all (uncurry commutes) pairs
 where
  pairs :: [(g, g)]
  pairs = [(x, y) | x <- elementsList, y <- elementsList]

$( singletons
     [d|
       data Foo = Foo {fooName :: String, fooCount :: Nat}
         deriving (Show, Eq)
       |]
 )

newtype Widget (f :: Foo) = Widget {widgetId :: Int}

label :: forall f. (SingI f) => Widget f -> String
label _ = case fromSing (sing @f) of
  Foo nm cnt -> nm ++ " x" ++ show cnt

myWidget :: Widget ('Foo (FromString "widget") 3)
myWidget = Widget{widgetId = 42}

newtype DihedralElement (n :: Nat) = DihedralElement (Bool, Int) deriving (Show, Eq, Ord)

instance (KnownNat n) => Semigroup (DihedralElement n) where
  (<>) (DihedralElement (b, x)) (DihedralElement (c, y)) =
    DihedralElement (xor b c, (if c then y - x + nat @n else y + x) `mod` nat @n)

instance (KnownNat n) => Monoid (DihedralElement n) where
  mempty = DihedralElement (False, 0)

instance (KnownNat n) => Group (DihedralElement n) (Bool, Int) where
  elements = Set.fromList $ DihedralElement <$> vals2
   where
    vals = [0 .. nat @n - 1]
    vals2 = [(b, x) | b <- [True, False], x <- vals]
  groupFrom = DihedralElement
  groupTo (DihedralElement bx) = bx
  inverse e@(DihedralElement (b, x)) =
    if b
      then e
      else
        DihedralElement (False, (nat @n - x) `mod` nat @n)

dihedralGroup :: Int -> (forall g. (Group g (Bool, Int {- Proxy g -> -})) => h) -> h
dihedralGroup n f =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy m) -> f @(DihedralElement m)

newtype Permutation (n :: Nat) = Permutation [Int] deriving (Show, Eq, Ord)

instance (KnownNat n) => Semigroup (Permutation n) where
  (<>) (Permutation p) (Permutation q) =
    Permutation ((p !!) <$> q)

instance (KnownNat n) => Monoid (Permutation n) where
  mempty = Permutation [0 .. nat @n - 1]

instance (KnownNat n) => Group (Permutation n) [Int] where
  elements = Set.fromList $ Permutation <$> vals
   where
    vals = permutations [0 .. nat @n - 1]
  groupFrom = Permutation
  groupTo (Permutation p) = p
  inverse (Permutation p) =
    let n = length p
        q = replicate n 0
     in Permutation $ foldr (\(i, j) acc -> take j acc ++ [i] ++ drop (j + 1) acc) q (zip [0 ..] p)

permutationGroup :: Int -> (forall g. (Group g [Int]) => h) -> h
permutationGroup n f =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy m) -> f @(Permutation m)

-- gcd :: Int -> Int -> Int
-- gcd m n
--   | m < n = gcd n m
--   | m == n = m
--   | otherwise = gcd (m `mod` n) n

newtype UnitMod (n :: Nat) = UnitMod Int deriving (Show, Eq, Ord)

instance (KnownNat n) => Semigroup (UnitMod n) where
  (<>) (UnitMod a) (UnitMod b) =
    UnitMod $ (a * b) `mod` (nat @n)

instance (KnownNat n) => Monoid (UnitMod n) where
  mempty = UnitMod 1

instance (KnownNat n) => Group (UnitMod n) Int where
  elements = Set.fromList $ UnitMod <$> vals
   where
    theN = nat @n
    vals = [ a | a <- [1 .. theN - 1], gcd a theN == 1 ]
  groupFrom = UnitMod
  groupTo (UnitMod a) = a
  inverse (UnitMod a) =
    case listToMaybe [ b | b <- [1 .. nat @n - 1], (a * b) `mod` nat @n== 1 ] of
      Just c -> UnitMod c
      _ -> UnitMod 1

unitsMod :: Int -> (forall g. (Group g Int) => h) -> h
unitsMod n f =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy m) -> f @(UnitMod m)
