{-# LANGUAGE NoImplicitPrelude #-}
module RIO.ListSpec where

import Test.Hspec
import RIO
import qualified RIO.List as List

spec :: Spec
spec = do
  describe "dropPrefix" $ do
    it "present" $ List.dropPrefix "foo" "foobar" `shouldBe` "bar"
    it "absent" $ List.dropPrefix "bar" "foobar" `shouldBe` "foobar"
  describe "dropSuffix" $ do
    it "present" $ List.dropSuffix "bar" "foobar" `shouldBe` "foo"
    it "absent" $ List.dropSuffix "foo" "foobar" `shouldBe` "foobar"
