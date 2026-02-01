module FastLane (ldaPlusOne, ldaAddThree) where

ldaPlusOne :: Int -> Int
ldaPlusOne = \x -> x + 1

ldaAddThree :: Int -> Int -> Int -> Int
ldaAddThree = \x y z -> x + y + z