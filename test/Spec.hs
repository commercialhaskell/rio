{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import RIO
import Data.ByteString.Builder (toLazyByteString)

main :: IO ()
main = hspec $ do
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
  describe "logger" $ do
    it "sanity" $ do
      ref <- newIORef mempty
      let options = LogOptions
            { logMinLevel = LevelInfo
            , logVerboseFormat = False
            , logTerminal = True
            , logUseTime = False
            , logUseColor = False
            , logSend = \builder -> modifyIORef ref (<> builder)
            }
      withStickyLogger options $ \lf -> runRIO lf $ do
        logDebug "should not appear"
        logInfo "should appear"
      builder <- readIORef ref
      toLazyByteString builder `shouldBe` "should appear\n"
    it "sticky" $ do
      ref <- newIORef mempty
      let options = LogOptions
            { logMinLevel = LevelInfo
            , logVerboseFormat = False
            , logTerminal = True
            , logUseTime = False
            , logUseColor = False
            , logSend = \builder -> modifyIORef ref (<> builder)
            }
      withStickyLogger options $ \lf -> runRIO lf $ do
        logSticky "ABC"
        logDebug "should not appear"
        logInfo "should appear"
        logStickyDone "XYZ"
      builder <- readIORef ref
      toLazyByteString builder `shouldBe` "ABC\b\b\b   \b\b\bshould appear\nABC\b\b\b   \b\b\bXYZ\n"
