{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module RIO.FileSpec where

import Test.Hspec
import UnliftIO.Temporary (withTempFile)

import RIO
import qualified RIO.ByteString as BS
import qualified RIO.File as SUT

genTempFilepath ::
  MonadUnliftIO m => FilePath -> String -> m FilePath
genTempFilepath dir tmpl = do
  withTempFile dir tmpl (\path _ -> return path)

spec :: Spec
spec = do
  describe "ensureFileDurable" $ do
    it "ensures a file is durable with an fsync" $ do
        let fp = "/tmp/ensure_file_durable"
        writeFileUtf8 fp "Hello World"
        SUT.ensureFileDurable fp
        contents <- BS.readFile fp
        contents `shouldBe` "Hello World"

  describe "withFileDurableAtomic" $ do
    context "read/write" $ do
      it "works correctly" $ do
        let fp = "/tmp/with_file_durable_atomic"
        writeFileUtf8 fp "Hello World"
        SUT.withFileDurableAtomic fp ReadWriteMode $ \h -> do
          input <- BS.hGetLine h
          input `shouldBe` "Hello World"
          BS.hPut h "Goodbye World"
        -- contents <- BS.readFile fp
        -- contents `shouldBe` "Goodbye World"

    context "happy path" $ do
      it "works the same as withFile" $ do
        let fp = "/tmp/with_file_durable_atomic"
        SUT.withFileDurableAtomic fp WriteMode $ \h ->
          BS.hPut h "Hello World"
        contents <- BS.readFile fp
        contents `shouldBe` "Hello World"

  describe "withFileDurable" $ do
    context "happy path" $ do
      it "works the same as withFile" $ do
        let fp = "/tmp/with_file_durable"
        SUT.withFileDurable fp WriteMode $ \h ->
          BS.hPut h "Hello World"
        contents <- BS.readFile fp
        contents `shouldBe` "Hello World"
