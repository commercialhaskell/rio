{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module RIO.PreludeSpec (spec) where

import Test.Hspec
import RIO
import Data.ByteString.Builder (toLazyByteString)

spec :: Spec
spec = do
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
