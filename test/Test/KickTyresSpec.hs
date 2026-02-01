module Test.KickTyresSpec where

import KickTyres
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
    describe "doubleMe" $ do
        it "doubles numbers" $
            doubleMe 3 `shouldBe` 6

    describe "tripleMe" $ do
        it "triples numbers" $
            tripleMe 4 `shouldBe` 12

        prop "double involution is identity" $ \n ->
            involve (involve n) == n

    describe "recursive functions" $ do
        it "e.g. Ackermann" $
            ack 3 3 `shouldBe` 61

    describe "tricks with numbers and strings" $ do
        it "can take min and max" $ do
            min (2 :: Int) 3 `shouldBe` 2

    describe "tricks with lists" $ do
        it "can cons up a list" $
            3 : [4 :: Int, 5] `shouldBe` [3, 4, 5]
        it "can cons up a list, strings being lists of characters" $
            'A' : " test" `shouldBe` "A test"
        it "can pick out elements from a list" $
            ["one", "two", "three"] !! 1 `shouldBe` "two"
        it "can concatenate lists" $
            [1 :: Int,2] ++ [3 :: Int,4] `shouldBe` [1 :: Int,2,3,4]
        it "can lexicographically compare lists" $
            [2 :: Int, 3, 4] < [2 :: Int, 5, 1] `shouldBe` True
        it "can define the fibonacci numbers" $
            take 8 fibs `shouldBe` ([0 :: Int,1,1,2,3,5,8,13] :: [Int])
        it "can test primality" $
            isPrime 11 `shouldBe` True
        it "can define the list of all primes" $
            take 4 allPrimes `shouldBe` [2,3,5,7]

