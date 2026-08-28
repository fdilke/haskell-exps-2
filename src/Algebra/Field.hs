{-# LANGUAGE MultilineStrings #-}
module Algebra.Field(FieldTable(..), FieldTableOps(..))
where
import Data.Text(Text)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy, unsnoc)
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)
import Utility.Utility
import Algebra.ConwayTable(conwayTable)

data FieldTable = FieldTable { 
    prime :: Int,
    power :: Int,
    primitive :: [Int]
  }
  deriving (Show, Eq)
class FieldTableOps ft where
    intToPoly :: ft -> Int -> [Int]
    polyToInt :: ft -> [Int] -> Int
    addPolys :: ft -> [Int] -> [Int] -> [Int]
    negPoly :: ft -> [Int] -> [Int]
    scalarMult :: ft -> Int -> [Int] -> [Int]
    shift :: ft -> [Int] -> [Int]
    mulPolys :: ft -> [Int] -> [Int] -> [Int]
    inversePoly :: ft -> [Int] -> [Int]
instance FieldTableOps FieldTable where
    intToPoly :: FieldTable -> Int -> [Int]
    intToPoly ft k = 
        intToPolySub k ft.power
        where
            intToPolySub k n =
                if n == 0 then []
                else (k `mod` ft.prime) : intToPolySub (k `div` ft.prime) (n-1)
    polyToInt :: FieldTable -> [Int] -> Int
    polyToInt ft poly = case poly of
        [] -> 0
        x : xs -> x + ft.prime * polyToInt ft xs
    addPolys :: FieldTable -> [Int] -> [Int] -> [Int]
    addPolys ft =
        zipWith \a b -> (a + b) `mod` ft.prime
    negPoly :: FieldTable -> [Int] -> [Int]
    negPoly ft p =
        p <&> (ft.prime -)
    scalarMult :: FieldTable -> Int -> [Int] -> [Int]
    scalarMult ft s p =
        p <&> \a -> (s * a) `mod` ft.prime
    shift :: FieldTable -> [Int] -> [Int]
    shift ft p = case unsnoc p of
        Nothing -> []
        Just (lait, x) -> addPolys ft (0 : lait) $ scalarMult ft (ft.prime - x) ft.primitive
    mulPolys :: FieldTable -> [Int] -> [Int] -> [Int]
    mulPolys ft p1 p2 =
        snd $ foldl combine (p1, zeroPoly) p2
        where
            zeroPoly = take ft.power (repeat 0)
            combine (shifted, sum) coefft =
                (shift ft shifted, addPolys ft sum $ scalarMult ft coefft shifted)
    inversePoly :: FieldTable -> [Int] -> [Int]
    inversePoly ft p =
        case ft.prime ^ ft.power of
        2 -> p
        pn -> associativeOpPower (mulPolys ft) p (pn - 2)

associativeOpPower :: (a -> a -> a) -> a -> Int -> a
associativeOpPower op x n
    | n == 1 = x
    | n `mod` 2 == 1 = op x (associativeOpPower op x (n -1))
    | otherwise = op y y where y = associativeOpPower op x (n `div` 2)

newtype FieldElement (pn :: Nat) = FieldElement Int deriving (Show, Eq, Ord)

instance (KnownNat pn) => Num (FieldElement pn) where
    (FieldElement a) + (FieldElement b) = FieldElement (
        polyToInt ft $
            addPolys ft (intToPoly ft a) (intToPoly ft b)
        ) where
            ft = getFieldTable (nat @pn)
    (FieldElement a) - (FieldElement b) = FieldElement (
        polyToInt ft $
            addPolys ft (intToPoly ft a) (negPoly ft $ intToPoly ft b)
        ) where
            ft = getFieldTable (nat @pn)
    negate (FieldElement a) = FieldElement (
        polyToInt ft $
            negPoly ft $ intToPoly ft a
        ) where
            ft = getFieldTable (nat @pn)
    (FieldElement a) * (FieldElement b) = FieldElement (
        polyToInt ft $
            mulPolys ft (intToPoly ft a) (intToPoly ft b)
        ) where
            ft = getFieldTable (nat @pn)
    abs x = x
    signum (FieldElement n) = FieldElement (signum n)
    fromInteger i = (FieldElement $ fromInteger i)

getFieldTable :: Int -> FieldTable
getFieldTable pn =
    FieldTable 5 2 [2,4] -- TODO: fix!

