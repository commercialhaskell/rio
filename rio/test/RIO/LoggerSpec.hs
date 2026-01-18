{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module RIO.LoggerSpec (spec) where

import Test.Hspec
import RIO
import Data.ByteString.Builder (toLazyByteString)
import qualified RIO.ByteString.Lazy as BS

spec :: Spec
spec = do
  it "sanity" $ do
    (ref, options) <- logOptionsMemory
    withLogFunc options $ \lf -> runRIO lf $ do
      logDebug "should not appear"
      logInfo "should appear"
    builder <- readIORef ref
    toLazyByteString builder `shouldBe` "should appear\n"
  it "setLogMinLevelIO" $ do
    (ref, options) <- logOptionsMemory
    logLevelRef <- newIORef LevelDebug
    withLogFunc (options & setLogMinLevelIO (readIORef logLevelRef))
      $ \lf -> runRIO lf $ do
        logDebug "should appear"
        -- reset log min level to info
        atomicModifyIORef' logLevelRef (const (LevelInfo, ()))
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
        atomicModifyIORef' logVerboseFormatRef (const (False, ()))
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
  it "noSource" $ do
    (ref, options) <- logOptionsMemory
    withLogFunc options $ \lf -> runRIO lf $ do
      logInfoS "tests" "should appear"
    builder <- readIORef ref
    toLazyByteString builder `shouldBe` "(tests) should appear\n"
  it "noSource verbose" $ do
    (ref, options) <- logOptionsMemory
    withLogFunc (options & setLogVerboseFormat True) $ \lf -> runRIO lf $ do
      logInfoS "tests" "should appear"
    builder <- readIORef ref
    toLazyByteString builder `shouldBe` "[info] (tests) should appear\n"
  it "setLogUseLoc" $ do
    (ref, options) <- logOptionsMemory
    withLogFunc (options & setLogUseLoc True) $ \lf -> runRIO lf $ do
      logInfo "location should follow"
    builder <- readIORef ref
    BS.take 25 (toLazyByteString builder) `shouldBe` "location should follow\n@("
  describe "logSticky" $ do
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
  describe "LogFormat" $ do
    it "setLogFormat" $ do
      (ref, options) <- logOptionsMemory
      let format = ("[context] " <>)
      withLogFunc (options & setLogFormat format) $ \lf -> runRIO lf $ do
        logInfo "should be formatted"
      builder <- readIORef ref
      toLazyByteString builder `shouldBe` "[context] should be formatted\n"
    it "source outside format" $ do
      (ref, options) <- logOptionsMemory
      let format = (<> ">") . ("<" <>)
      withLogFunc (options & setLogFormat format) $ \lf -> runRIO lf $ do
        logInfoS "tests" "should be bracketed"
      builder <- readIORef ref
      toLazyByteString builder `shouldBe` "(tests) <should be bracketed>\n"
    it "location outside format" $ do
      (ref, options) <- logOptionsMemory
      let format = (<> ">") . ("<" <>)
      withLogFunc (options & setLogUseLoc True & setLogFormat format) $ \lf -> runRIO lf $ do
        logInfo "location should follow"
      builder <- readIORef ref
      BS.take 27 (toLazyByteString builder) `shouldBe` "<location should follow>\n@("
    it "composeLogFormat" $ do
      (ref, options) <- logOptionsMemory
      let format = (<> ">") . ("<" <>)
          composeInner = (. ("inner: " <>))
          composeOuter = (("[outer] " <>) .)
          options' = options & setLogFormat format
                            & composeLogFormat composeInner
                            & composeLogFormat composeOuter
      withLogFunc options' $ \lf -> runRIO lf $ do
        logInfo "should be bracketed"
      builder <- readIORef ref
      toLazyByteString builder `shouldBe` "[outer] <inner: should be bracketed>\n"
    it "withLocalLogFunc" $ do
      (ref, options) <- logOptionsMemory
      withLogFunc options $ \lf -> runRIO lf $ do
        let composeLocal = composeLogFormat (("[local] " <>) .)
        withLocalLogFunc composeLocal $ do
          logInfo "should be formatted"
        logInfo "should not be formatted"
      builder <- readIORef ref
      toLazyByteString builder `shouldBe` "[local] should be formatted\nshould not be formatted\n"
