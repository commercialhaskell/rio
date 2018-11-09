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
  describe "withFileDurableAtomic" $ do
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
