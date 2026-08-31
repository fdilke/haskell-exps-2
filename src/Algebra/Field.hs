{-# LANGUAGE MultilineStrings #-}
module Algebra.Field(FieldTable(..), fieldTable)
where
import Data.Text(Text)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy, unsnoc)
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)
import Utility.Utility
import Data.Map (Map)
import Data.Map qualified as Map

data FieldTable = FieldTable { 
    addTable :: Map (Int, Int) Int,
    mulTable :: Map (Int, Int) Int,
    negTable :: Map Int Int,
    invTable :: Map Int Int
  }
  deriving (Show, Eq)

fieldTable :: Int -> Int -> [Int] -> FieldTable
fieldTable prime power primitive =
    FieldTable {
        addTable = addTable,
        negTable = negTable,
        mulTable = mulTable,
        invTable = invTable
    } where
        pn = prime ^ power
        domain = [0..(pn-1)]
        smartNeg x =
            if x == 0 then 0 else prime - x
        addTable = tabulate2 domain domain \i j ->
            polyToInt $
                addPolys (intToPoly i) (intToPoly j)
        mulTable = tabulate2 domain domain \i j ->
            polyToInt $
                mulPolys (intToPoly i) (intToPoly j)
        invTable = tabulate domain $ polyToInt . invPoly . intToPoly
        negTable = tabulate domain $ polyToInt . negPoly . intToPoly
        intToPoly :: Int -> [Int]
        intToPoly k = 
            intToPolySub k power
            where
                intToPolySub l n =
                    if n == 0 then []
                    else (l `mod` prime) : intToPolySub (l `div` prime) (n-1)
        polyToInt :: [Int] -> Int
        polyToInt = \case
            [] -> 0
            x : xs -> x + prime * polyToInt xs
        addPolys :: [Int] -> [Int] -> [Int]
        addPolys =
            zipWith \a b -> (a + b) `mod` prime
        negPoly :: [Int] -> [Int]
        negPoly p =
            p <&> smartNeg
        scalarMult :: Int -> [Int] -> [Int]
        scalarMult s p =
            p <&> \a -> (s * a) `mod` prime
        shift :: [Int] -> [Int]
        shift p = case unsnoc p of
            Nothing -> []
            Just (lait, x) -> addPolys (0 : lait) $ scalarMult (smartNeg x) primitive
        mulPolys :: [Int] -> [Int] -> [Int]
        mulPolys p1 p2 =
            snd $ foldl combine (p1, zeroPoly) p2
            where
                zeroPoly = replicate power 0
                combine (shifted, sum) coefft =
                    (shift shifted, addPolys sum $ scalarMult coefft shifted)
        invPoly :: [Int] -> [Int]
        invPoly p =
            case pn of
            2 -> p
            _ -> associativeOpPower mulPolys p (pn - 2)

newtype FieldElement (pn :: Nat) = FieldElement Int deriving (Show, Eq, Ord)

instance (KnownNat pn) => Num (FieldElement pn) where
    (FieldElement a) + (FieldElement b) = FieldElement (
        ft.addTable Map.! (a, b) 
        ) where
            ft = getFieldTable (nat @pn)
    (FieldElement a) - (FieldElement b) = FieldElement (
        ft.mulTable Map.! (a, b) 
        ) where
            ft = getFieldTable (nat @pn)
    negate (FieldElement a) = FieldElement (
        ft.invTable Map.! a
        ) where
            ft = getFieldTable (nat @pn)
    (FieldElement a) * (FieldElement b) = FieldElement (
        ft.mulTable Map.! (a, b) 
        ) where
            ft = getFieldTable (nat @pn)
    abs x = x
    signum (FieldElement n) = FieldElement (signum n)
    fromInteger i = FieldElement $ fromInteger i

getFieldTable :: Int -> FieldTable
getFieldTable pn =
    fieldTable 5 2 [2,4] -- TODO: fix!

