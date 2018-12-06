{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module RIO.FileSpec where

import Test.Hspec
import System.FilePath ((</>))
import UnliftIO.Temporary (withSystemTempDirectory)

import RIO
import qualified RIO.ByteString as BS
import qualified RIO.File as SUT

spec :: Spec
spec = do
  describe "ensureFileDurable" $ do
    it "ensures a file is durable with an fsync" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> "ensure_file_durable"
        writeFileUtf8 fp "Hello World"
        SUT.ensureFileDurable fp
        contents <- BS.readFile fp
        contents `shouldBe` "Hello World"

  describe "withBinaryFileDurableAtomic" $ do
    context "read/write" $ do
      it "works correctly" $ do
        withSystemTempDirectory "rio" $ \dir -> do
          let fp = dir </> "ensure_file_durable_atomic"
          writeFileUtf8 fp "Hello World"
          SUT.withBinaryFileDurableAtomic fp ReadWriteMode $ \h -> do
            input <- BS.hGetLine h
            input `shouldBe` "Hello World"
            BS.hPut h "Goodbye World"

    context "happy path" $ do
      it "works the same as withFile" $ do
        withSystemTempDirectory "rio" $ \dir -> do
          let fp = dir </> "with_file_durable_atomic"
          SUT.withBinaryFileDurableAtomic fp WriteMode $ \h ->
            BS.hPut h "Hello World"
          contents <- BS.readFile fp
          contents `shouldBe` "Hello World"

  describe "withBinaryFileDurable" $ do
    context "happy path" $ do
      it "works the same as withFile" $ do
        withSystemTempDirectory "rio" $ \dir -> do
          let fp = dir </> "with_file_durable"
          SUT.withBinaryFileDurable fp WriteMode $ \h ->
            BS.hPut h "Hello World"
          contents <- BS.readFile fp
          contents `shouldBe` "Hello World"
