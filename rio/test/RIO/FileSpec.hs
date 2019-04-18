{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RIO.FileSpec where

import Test.Hspec
import RIO

-- Atomic/durable file writing is not supported on Windows.
#ifndef WINDOWS
import System.FilePath ((</>))
import Test.QuickCheck
import UnliftIO.Directory
import UnliftIO.Temporary (withSystemTempDirectory)

import qualified RIO.ByteString as BS
import qualified RIO.File as File

data ExpectedException =
  ExpectedException
  deriving (Show)

instance Exception ExpectedException

spec :: Spec
spec = do
  describe "ensureFileDurable" $
    it "ensures a file is durable with an fsync" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> "ensure_file_durable"
        writeFileUtf8 fp "Hello World"
        File.ensureFileDurable fp
        contents <- BS.readFile fp
        contents `shouldBe` "Hello World"
  withBinaryFileSpec False "withBinaryFile" withBinaryFile
  writeBinaryFileSpec "writeBinaryFile" writeFileBinary
  -- Above two specs are validating the specs behavior by applying to
  -- known good implementations
  withBinaryFileSpec True "withBinaryFileAtomic" File.withBinaryFileAtomic
  writeBinaryFileSpec "writeBinaryFileAtomic" File.writeBinaryFileAtomic
  withBinaryFileSpec False "withBinaryFileDurable" File.withBinaryFileDurable
  writeBinaryFileSpec "writeBinaryFileDurable" File.writeBinaryFileDurable
  withBinaryFileSpec True "withBinaryFileDurableAtomic" File.withBinaryFileDurableAtomic
  writeBinaryFileSpec "writeBinaryFileDurableAtomic" File.writeBinaryFileDurableAtomic

withBinaryFileSpec ::
     Bool -- ^ Should we test atomicity
  -> String
  -> (forall a. FilePath -> IOMode -> (Handle -> IO a) -> IO a)
  -> Spec
withBinaryFileSpec atomic fname withFileTestable = do
  let hello = "Hello World"
      writeHello fp = writeFileUtf8Builder fp $ displayBytesUtf8 hello
      -- Create a file, write "Hello World" into it and apply the action.
      withHelloFileTestable fp iomode action = do
        writeHello fp
        withFileTestable fp iomode action
      goodbye = "Goodbye yall"
      modifiedPermissions =
        setOwnerExecutable True $
        setOwnerReadable True $ setOwnerWritable True emptyPermissions
  describe fname $ do
    it "read" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-read"
        withHelloFileTestable fp ReadWriteMode (`BS.hGet` BS.length hello) `shouldReturn`
          hello
    it "write" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-write"
        withHelloFileTestable fp WriteMode (`BS.hPut` goodbye)
        BS.readFile fp `shouldReturn` goodbye
    it "read/write" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-read-write"
        withHelloFileTestable fp ReadWriteMode $ \h -> do
          BS.hGetLine h `shouldReturn` hello
          BS.hPut h goodbye
        BS.readFile fp `shouldReturn` (hello <> goodbye)
    it "append" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-append"
            privet = "Привет Мир" -- some unicode won't hurt
        writeFileUtf8Builder fp $ display privet
        setPermissions fp modifiedPermissions
        withFileTestable fp AppendMode $ \h -> BS.hPut h goodbye
        BS.readFile fp `shouldReturn` (encodeUtf8 privet <> goodbye)
    it "sub-directory" $
      withSystemTempDirectory "rio" $ \dir -> do
        let subDir = dir </> fname ++ "-sub-directory"
            fp = subDir </> "test.file"
        createDirectoryIfMissing True subDir
        withHelloFileTestable fp ReadWriteMode $ \h -> do
          BS.hGetLine h `shouldReturn` hello
          BS.hPut h goodbye
        BS.readFile fp `shouldReturn` (hello <> goodbye)
    it "relative-directory" $
      withSystemTempDirectory "rio" $ \dir -> do
        let relDir = fname ++ "-relative-directory"
            subDir = dir </> relDir
            fp = relDir </> "test.file"
        createDirectoryIfMissing True subDir
        withCurrentDirectory dir $ do
          withHelloFileTestable fp ReadWriteMode $ \h -> do
            BS.hGetLine h `shouldReturn` hello
            BS.hPut h goodbye
          BS.readFile fp `shouldReturn` (hello <> goodbye)
    it "modified-permissions" $
      forM_ [WriteMode, ReadWriteMode, AppendMode] $ \iomode ->
        withSystemTempDirectory "rio" $ \dir -> do
          let fp = dir </> fname ++ "-modified-permissions"
          writeHello fp
          setPermissions fp modifiedPermissions
          withFileTestable fp iomode $ \h -> BS.hPut h goodbye
          getPermissions fp `shouldReturn` modifiedPermissions
    it "exception - Does not corrupt files" $
      bool expectFailure id atomic $ -- should fail for non-atomic
      forAll (elements [WriteMode, ReadWriteMode, AppendMode]) $ \iomode ->
        withSystemTempDirectory "rio" $ \dir -> do
          let fp = dir </> fname ++ "-exception"
          _ :: Either ExpectedException () <-
            try $
            withHelloFileTestable fp iomode $ \h -> do
              BS.hPut h goodbye
              throwIO ExpectedException
          BS.readFile fp `shouldReturn` hello
    it "exception - Does not leave files behind" $
      bool expectFailure id atomic $ -- should fail for non-atomic
      forAll (elements [WriteMode, ReadWriteMode, AppendMode]) $ \iomode ->
        withSystemTempDirectory "rio" $ \dir -> do
          let fp = dir </> fname ++ "-exception"
          _ :: Either ExpectedException () <-
            try $
            withFileTestable fp iomode $ \h -> do
              BS.hPut h goodbye
              throwIO ExpectedException
          doesFileExist fp `shouldReturn` False
          listDirectory dir `shouldReturn` []
    it "delete - file" $
      bool expectFailure id atomic $ -- should fail for non-atomic
      forAll (elements [WriteMode, ReadWriteMode, AppendMode]) $ \iomode ->
        withSystemTempDirectory "rio" $ \dir -> do
          let fp = dir </> fname ++ "-delete"
          withHelloFileTestable fp iomode $ \h -> do
            removeFile fp
            BS.hPut h goodbye
          doesFileExist fp `shouldReturn` True

writeBinaryFileSpec :: String -> (FilePath -> ByteString -> IO ()) -> SpecWith ()
writeBinaryFileSpec fname writeFileTestable = do
  let hello = "Hello World"
      defaultPermissions =
        setOwnerReadable True $ setOwnerWritable True emptyPermissions
  describe fname $ do
    it "write" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-write"
        writeFileTestable fp hello
        BS.readFile fp `shouldReturn` hello
    it "default-permissions" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-default-permissions"
        writeFileTestable fp hello
        getPermissions fp `shouldReturn` defaultPermissions
#else
spec :: Spec
spec = pure ()
#endif
