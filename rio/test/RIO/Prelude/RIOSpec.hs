{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module RIO.Prelude.IOSpec (spec) where

import RIO
import Test.Hspec
import Test.Hspec.QuickCheck

spec =
  describe "RIO writer instance" $ do
    it "tell works" $ do
      ref <- newSomeRef (mempty :: Text)
      runRIO ref $ do
        tell "hello\n"
        tell "world\n"
      contents <- readSomeRef ref
      contents `shouldBe` "hello\nworld\n"

    -- it "listen works" (return ())
    -- it "pass works" (return ())

  -- describe "RIO state instance" $ do
  --   it "get works"
  --   it "modify works"
  --   it "set works"
