{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module RIO.PreludeSpec (spec) where

import Test.Hspec
import RIO

spec :: Spec
spec = do
  describe "URef" $ do
    it "sanity" $ do
      ref <- newURef (0 :: Int)
      x <- readURef ref
      x `shouldBe` 0
      writeURef ref 1
      y <- readURef ref
      y `shouldBe` 1
      modifyURef ref (+ 1)
      z <- readURef ref
      z `shouldBe` 2
  describe "whenM" $ do
    it "returns True" $ do
      ref <- newIORef False
      whenM (return True) (writeIORef ref True)
      readIORef ref `shouldReturn` True
    it "returns False" $ do
      ref <- newIORef False
      whenM (return False) (writeIORef ref True)
      readIORef ref `shouldReturn` False
  describe "unlessM" $ do
    it "returns True" $ do
      ref <- newIORef False
      unlessM (return True) (writeIORef ref True)
      readIORef ref `shouldReturn` False
    it "returns False" $ do
      ref <- newIORef False
      unlessM (return False) (writeIORef ref True)
      readIORef ref `shouldReturn` True
