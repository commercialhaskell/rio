{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module RIO.Prelude.RIOSpec (spec) where

import RIO
import RIO.State
import RIO.Writer
import Test.Hspec

spec :: Spec
spec = do
  describe "RIO writer instance" $ do
    it "tell works" $ do
     ref <- newSomeRef (mempty :: Text)
     runRIO ref $ do
       tell "hello\n"
       tell "world\n"
     contents <- readSomeRef ref
     contents `shouldBe` "hello\nworld\n"

    it "listen works" $ do
      ref <- newSomeRef (mempty :: Text)
      ((), str) <- runRIO ref $ listen $ do
        tell "hello\n"
        tell "world\n"
      contents <- readSomeRef ref
      contents `shouldBe` ""
      str `shouldBe` "hello\nworld\n"

    it "pass works" $ do
      ref <- newSomeRef (mempty :: Text)
      () <- runRIO ref $ pass $ do
        tell "hello\n"
        tell "world\n"
        return ((), (<> "!"))
      contents <- readSomeRef ref
      contents `shouldBe` "hello\nworld\n!"

  describe "RIO state instance" $ do
    it "get works" $ do
      ref <- newSomeRef (mempty :: Text)
      result <- runRIO ref $ do
        put "hello world"
        get
      result `shouldBe` "hello world"

    it "state works" $ do
      ref <- newSomeRef (mempty :: Text)
      _newRef <- newSomeRef ("Hello World!" :: Text)
      () <- runRIO ref $ state (const ((), "Hello World!"))
      contents <- readSomeRef ref
      contents `shouldBe` "Hello World!"
