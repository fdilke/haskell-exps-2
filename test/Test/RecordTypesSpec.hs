module Test.RecordTypesSpec where

import RecordTypes
import Test.Hspec
import Test.Hspec.QuickCheck ()

spec :: Spec
spec = do
    describe "Custom record types" $ do
        it "can encapsulate and test compound objects" $ do
            let person = Person { name="Felix", shoeSize=7 }
            hasBigFeet person `shouldBe` False