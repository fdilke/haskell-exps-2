module Test.GroupSpec where

import Test.Hspec

-- import Newton

spec :: Spec
spec = do
  describe "Group" $ do
    it "can define a cyclic group" $
      True `shouldBe` True
