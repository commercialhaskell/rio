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
  it "relodable" $ do
    (ref, options) <- logOptionsMemory
    withReloadableLogFunc (options & setLogMinLevel LevelDebug)
      $ \(lf, reload) -> runRIO lf $ do
        logDebug "should appear"
        -- reset log min level to info
        liftIO $ reload options
        logDebug "should not appear"
    builder <- readIORef ref
    toLazyByteString builder `shouldBe` "should appear\n"
