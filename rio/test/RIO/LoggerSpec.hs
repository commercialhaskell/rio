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
