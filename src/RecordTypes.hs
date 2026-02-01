module RecordTypes where

data Person = Person { name :: String
                     , shoeSize :: Int
                     } deriving (Show)

hasBigFeet :: Person -> Bool
hasBigFeet person = shoeSize person > 10
