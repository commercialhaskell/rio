{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module RIO.Prelude.RIOSpec (spec) where

import RIO
import RIO.State
import RIO.Writer
import Test.Hspec
import Test.Hspec.QuickCheck


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
      result <- runRIO ref $ pass $ do
        tell "hello\n"
        tell "world\n"
        return ((), \a -> a <> "!")
      contents <- readSomeRef ref
      contents `shouldBe` "hello\nworld\n!"

  describe "RIO state instance" $ do
    it "get works" $ do
      ref <- newSomeRef (mempty :: Text)
      result <- runRIO ref $ do
        put "hello world"
        x <- get
        return x
      result `shouldBe` "hello world"

    it "state works" $ do
      ref <- newSomeRef (mempty :: Text)
      newRef <- newSomeRef ("Hello World!" :: Text)
      result <- runRIO ref $ state (\ref -> ((), "Hello World!"))
      contents <- readSomeRef ref
      contents `shouldBe` "Hello World!"
