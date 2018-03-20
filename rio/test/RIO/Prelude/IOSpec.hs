{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module RIO.Prelude.IOSpec (spec) where

import RIO
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified RIO.ByteString as B
import qualified RIO.Text as T

spec :: Spec
spec = do
  prop "binary file read/write" $ \(B.pack -> bs1) ->
    withSystemTempFile "binary-read-write" $ \fp h -> do
      hClose h
      writeFileBinary fp bs1
      bs2 <- readFileBinary fp
      bs2 `shouldBe` bs1
  -- filter our \r for Windows
  prop "text file read/write" $ \(T.pack . filter (/= '\r') -> text1) ->
    withSystemTempFile "binary-read-write" $ \fp h -> do
      hClose h
      writeFileUtf8 fp text1
      text2 <- readFileUtf8 fp
      text2 `shouldBe` text1
