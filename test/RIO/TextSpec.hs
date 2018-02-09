{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module RIO.TextSpec where

import Test.Hspec
import RIO
import qualified RIO.Text as T

spec :: Spec
spec = do
  describe "dropPrefix" $ do
    it "present" $ T.dropPrefix "foo" "foobar" `shouldBe` "bar"
    it "absent" $ T.dropPrefix "bar" "foobar" `shouldBe` "foobar"
  describe "dropSuffix" $ do
    it "present" $ T.dropSuffix "bar" "foobar" `shouldBe` "foo"
    it "absent" $ T.dropSuffix "foo" "foobar" `shouldBe` "foobar"
