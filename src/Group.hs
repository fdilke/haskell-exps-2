{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Group (
  Group (..),
  cyclicGroup,
  dihedralGroup,
  orderElement,
  orderGroup,
  isAbelian,
) where

import Data.Bits (xor)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Singletons.Base.TH
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)

class (Eq g, Ord g, Show g) => Group g p | g -> p where
  elements :: Set g
  groupFrom :: p -> g
  groupTo :: g -> p
  identity :: g
  inverse :: g -> g
  combine :: g -> g -> g
  elementsList :: [g]
  elementsList = Set.toList elements

nat :: forall n. (KnownNat n) => Int
nat = fromIntegral (natVal (Proxy @n))

-- | The cyclic group Z/nZ, with the nat @n@ carried at the type level.
newtype Power (n :: Nat) = Power Int deriving (Show, Eq, Ord)

instance (KnownNat n) => Group (Power n) Int where
  elements = Set.fromList $ Power <$> [0 .. nat @n - 1]
  groupFrom = Power
  groupTo (Power x) = x
  identity = Power 0
  inverse (Power x) = Power ((nat @n - x) `mod` nat @n)
  combine (Power x) (Power y) = Power ((x + y) `mod` nat @n)

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
    | y == identity = n
    | otherwise = test (n + 1) (combine x y)

commutes :: forall g p. (Group g p) => g -> g -> Bool
commutes x y = combine x y == combine y x

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

instance (KnownNat n) => Group (DihedralElement n) (Bool, Int) where
  elements = Set.fromList $ DihedralElement <$> vals2
   where
    vals = [0 .. nat @n - 1]
    vals2 = [(b, x) | b <- [True, False], x <- vals]
  groupFrom = DihedralElement
  groupTo (DihedralElement bx) = bx
  identity = DihedralElement (False, 0)
  inverse e@(DihedralElement (b, x)) =
    if b
      then e
      else
        DihedralElement (False, (nat @n - x) `mod` nat @n)
  combine (DihedralElement (b, x)) (DihedralElement (c, y)) =
    DihedralElement (xor b c, (if c then y - x + nat @n else y + x) `mod` nat @n)

dihedralGroup :: Int -> (forall g. (Group g (Bool, Int {- Proxy g -> -})) => h) -> h
dihedralGroup n f =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy m) -> f @(DihedralElement m)
