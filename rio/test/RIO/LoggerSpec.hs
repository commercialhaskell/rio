{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module RIO.LoggerSpec (spec) where

import Test.Hspec
import RIO
import Data.ByteString.Builder (toLazyByteString)

spec :: Spec
spec = do
  it "sanity" $ do
    (ref, options) <- logOptionsMemory
    withLogFunc options $ \lf -> runRIO lf $ do
      logDebug "should not appear"
      logInfo "should appear"
    builder <- readIORef ref
    toLazyByteString builder `shouldBe` "should appear\n"
  it "sticky" $ do
    (ref, options) <- logOptionsMemory
    withLogFunc options $ \lf -> runRIO lf $ do
      logSticky "ABC"
      logDebug "should not appear"
      logInfo "should appear"
      logStickyDone "XYZ"
    builder <- readIORef ref
    toLazyByteString builder `shouldBe` "ABC\b\b\b   \b\b\bshould appear\nABC\b\b\b   \b\b\bXYZ\n"
  it "stickyUnicode" $ do
    (ref, options) <- logOptionsMemory
    withLogFunc options $ \lf -> runRIO lf $ do
      logSticky "ö"
      logStickyDone "."
    builder <- readIORef ref
    toLazyByteString builder `shouldBe` "\195\182\b \b.\n"
  it "stickyAnsiEscape" $ do
    (ref, options) <- logOptionsMemory
    withLogFunc options $ \lf -> runRIO lf $ do
      logSticky "\ESC[31mABC\ESC[0m"
      logStickyDone "."
    builder <- readIORef ref
    toLazyByteString builder `shouldBe` "\ESC[31mABC\ESC[0m\b\b\b   \b\b\b.\n"
  it "setLogMinLevelIO" $ do
    (ref, options) <- logOptionsMemory
    logLevelRef <- newIORef LevelDebug
    withLogFunc (options & setLogMinLevelIO (readIORef logLevelRef))
      $ \lf -> runRIO lf $ do
        logDebug "should appear"
        -- reset log min level to info
        atomicModifyIORef' logLevelRef (\_ -> (LevelInfo, ()))
        logDebug "should not appear"
    builder <- readIORef ref
    toLazyByteString builder `shouldBe` "should appear\n"
  it "setLogVerboseFormatIO" $ do
    (ref, options) <- logOptionsMemory
    logVerboseFormatRef <- newIORef True
    withLogFunc (options & setLogVerboseFormatIO (readIORef logVerboseFormatRef))
      $ \lf -> runRIO lf $ do
        logInfo "verbose log"
        -- reset verbose format
        atomicModifyIORef' logVerboseFormatRef (\_ -> (False, ()))
        logInfo "no verbose log"
    builder <- readIORef ref
    toLazyByteString builder `shouldBe` "[info] verbose log\nno verbose log\n"
  it "noLogging" $ do
    (ref, options) <- logOptionsMemory
    withLogFunc (options & setLogVerboseFormat True) $ \lf -> runRIO lf $ do
      logInfo "should appear"
      noLogging $ logInfo "should not appear"
    builder <- readIORef ref
    toLazyByteString builder `shouldBe` "[info] should appear\n"
