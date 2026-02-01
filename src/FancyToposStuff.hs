{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module FancyToposStuff where

import Data.Set

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

class Topos t dot arrow where
    id :: dot x -> arrow x x
    compose :: arrow x y -> arrow y z -> arrow x z

data FiniteSetsDot a where
  Set :: a -> FiniteSetsDot a

data FiniteSetsArrow a b = FiniteSetsArrow {
    source :: FiniteSetsDot a,
    target :: FiniteSetsDot b,
    function :: a -> b
}


--class Dot d where
--    id :: d x -> a x x

-- instance Topos FiniteSetsDot FiniteSetsArrow where

--class Arrow a where
--    o :: a y z -> a x z

--class Dot d where
--    id :: d x -> a x x
--
--class Functor f => Applicative f where
--  pure  :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b

--data Set a = Set [a] deriving (Show)
--
--empty :: Set a
--empty = Set []
--
--insert :: (Eq a) => a -> Set a -> Set a
--insert x (Set xs)
--    | not (x `elem` xs) = Set (x : xs)
--    | otherwise         = Set xs



main = do
    putStrLn("Hello World main ")
--    let set = Set [1,2,3]
--    putStrLn(show $ insert 5 set)
--    val dot = new Dot("*")
--    val i = dot.identity
--    println("true?" + (i o i == i))

