module KickTyres (doubleMe, tripleMe, involve, ack, fibs, isPrime, allPrimes) where

doubleMe :: Int -> Int
doubleMe n = n + n

tripleMe :: Int -> Int
tripleMe n = n + n + n

involve :: Int -> Int
involve n = 1 - n

ack :: Int -> Int -> Int
ack 0 n = n + 1
ack m 0 = ack (m - 1) 1
ack m n = ack (m - 1) (ack m (n - 1))

fibs :: [Int]
fibs = 0 : zipWith (+) fibs (1 : fibs)

allPrimes :: [Int]
allPrimes = filter isPrime [1..]

isPrime :: Int -> Bool
isPrime n = (n > 1 &&) $
    null $ filter isFactor [2..pred n]
        where isFactor = (0 ==) . (mod n)
