{-# LANGUAGE NoImplicitPrelude #-}
module RIO.VectorSpec where

import Test.Hspec
import RIO
import qualified RIO.Vector.Boxed as VB

spec :: Spec
spec =
  describe "sliceMaybe" $ do
    it "fails on negative index" $
      VB.sliceMaybe (-1) 0 v `shouldBe` Nothing
    it "fails on negative slice length" $
      VB.sliceMaybe 0 (-1) v `shouldBe` Nothing
    it "fails when index too large" $
      VB.sliceMaybe (VB.length v + 1) 0 v `shouldBe` Nothing
    it "fails when slice too large" $
      VB.sliceMaybe 0 (VB.length v + 1) v `shouldBe` Nothing
    it "works for all of vector" $
      VB.sliceMaybe 0 (VB.length v) v `shouldBe` Just v
    it "works for zero slice length" $
      VB.sliceMaybe 0 0 v `shouldBe` Just mempty
    it "works for zero vector" $
      VB.sliceMaybe 0 0 (mempty :: VB.Vector ()) `shouldBe` Just mempty
    it "works for end of vector" $
      VB.sliceMaybe (VB.length v) 0 v `shouldBe` Just mempty
  where
    v = VB.fromList "foobar"
