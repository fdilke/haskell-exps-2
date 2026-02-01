{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeApplications #-}

module Test.OlogsSpec where

import Data.Either (isRight)
import Ologs
import Test.Hspec

type MaybeOlog = Either (MakeOlogError Int) (Olog Int)

spec :: Spec
spec = do
  describe "olog sanity checks" $ do
    it "arcs must have a source of a known dot" $
      let badOlog :: MaybeOlog
          badOlog =
            makeOlog [1] [("source", 0, 1)] []
       in badOlog `shouldBe` Left (UnknownSource "source" 0)
    it "arcs must have a target of a known dot" $
      let badOlog :: MaybeOlog
          badOlog =
            makeOlog [0] [("source", 0, 1)] []
       in badOlog `shouldBe` Left (UnknownTarget "source" 1)
    it "arc is then ok" $
      let goodOlog :: MaybeOlog
          goodOlog =
            makeOlog [0, 1] [("y", 1, 0), ("x", 0, 1)] []
       in isRight goodOlog `shouldBe` True
    it "identities can't just say 1 = 1" $
      let badOlog :: MaybeOlog
          badOlog =
            makeOlog [0] [] [([], [])]
       in badOlog `shouldBe` Left ForbiddenTrivialIdentity
    it "identities should only use known names" $
      let badOlog :: MaybeOlog
          badOlog =
            makeOlog [0] [] [(["identity"], [])]
     in badOlog `shouldBe` Left (UnknownArc "identity")
    it "arcs in lhs of identities join up" $
      let badOlog :: MaybeOlog
          badOlog =
            makeOlog
              [0, 1, 2]
              [("0to1", 0, 1), ("1to2", 1, 2), ("0to2", 0, 2), ("1to0", 1, 0)]
              [(["0to1", "1to2"], ["0to2"])]
       in badOlog `shouldBe` Left (NonJoiningExpressionLhs ["0to1", "1to2"])
    it "arcs in rhs of identities join up" $
      let badOlog :: MaybeOlog
          badOlog =
            makeOlog
              [0, 1, 2]
              [("0to1", 0, 1), ("1to2", 1, 2), ("0to2", 0, 2), ("1to0", 1, 0)]
              [(["0to2"], ["0to1", "1to2"])]
       in badOlog `shouldBe` Left (NonJoiningExpressionRhs ["0to1", "1to2"])
    it "lhs and rhs of identities have same source and target" $
      let badOlog :: MaybeOlog
          badOlog =
            makeOlog
              [0, 1, 2]
              [("0to1", 0, 1), ("1to2", 1, 2), ("0to2", 0, 2), ("1to0", 1, 0)]
              [(["1to0", "0to1"], ["1to2", "0to1"])]
       in badOlog `shouldBe` Left (IdentityMismatch ["1to0", "0to1"] ["1to2", "0to1"] (0, 0) (0, 2))
    it "consistent joined-up identities are ok" $
      let goodOlog :: MaybeOlog
          goodOlog =
            makeOlog
              [0, 1, 2]
              [("0to1", 0, 1), ("1to2", 1, 2), ("0to2", 0, 2), ("1to0", 1, 0)]
              [(["1to2", "0to1"], ["0to2"])]
       in 
        case goodOlog of
          Left err -> expectationFailure $ show err
          Right _ -> pure ()

-- it "identities should have the same source and target on both sides" $
-- describe "create a basic olog" $ do
--   it "the graph olog" $
--     let -- x :: Int
--         -- x = 3
--         graphOlog :: Either String (Olog Int)
--         graphOlog =
--           makeOlog [0, 1] [("source", 0, 1), ("target", 1, 0)] []
--     in do
--           [7, 8] `shouldBe` [7, 8]
-- TODO: add test when [] == something that isn't a loop
