{-# LANGUAGE NoImplicitPrelude #-}
module RIO.ListSpec where

import Test.Hspec
import RIO
import qualified RIO.List as List

newtype TestType = TestType { testTypeContents :: Int }
  deriving (Eq, Show)

testTypeList :: [TestType]
testTypeList = [TestType { testTypeContents = 1 }, TestType { testTypeContents = 0 }]

spec :: Spec
spec = do
  describe "dropPrefix" $ do
    it "present" $ List.dropPrefix "foo" "foobar" `shouldBe` "bar"
    it "absent" $ List.dropPrefix "bar" "foobar" `shouldBe` "foobar"
  describe "dropSuffix" $ do
    it "present" $ List.dropSuffix "bar" "foobar" `shouldBe` "foo"
    it "absent" $ List.dropSuffix "foo" "foobar" `shouldBe` "foobar"
  describe "maximumByMaybe" $ do
    it "should support elements that do not have an Ord instance" $
      List.maximumByMaybe (compare `on` testTypeContents) testTypeList `shouldBe` Just TestType { testTypeContents = 1}
  describe "minimumByMaybe" $ do
    it "should support elements that do not have an Ord instance" $
      List.minimumByMaybe (compare `on` testTypeContents) testTypeList `shouldBe` Just TestType { testTypeContents = 0}
