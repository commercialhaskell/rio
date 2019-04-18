{-# LANGUAGE CPP #-}
module RIO.Prelude.IO
  ( withBinaryFileLazy
  , readBinaryFile
  , writeBinaryFile
  , readFileUtf8
  , writeFileUtf8
  , hPutBuilder
  , withLazyFile
  , readFileBinary
  , writeFileBinary
  ) where

import           RIO.Prelude.Reexports
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text.IO            as T
import           System.IO               (hSetEncoding, utf8)

-- | Lazily get the contents of a file. Unlike 'BL.readFile', this
-- ensures that if an exception is thrown, the file handle is closed
-- immediately.
--
-- @since 0.1.10.0
withBinaryFileLazy :: MonadUnliftIO m => FilePath -> (BL.ByteString -> m a) -> m a
withBinaryFileLazy fp inner = withBinaryFile fp ReadMode $ inner <=< liftIO . BL.hGetContents

-- | Synonym for `withBinaryFileLazy`
withLazyFile :: MonadUnliftIO m => FilePath -> (BL.ByteString -> m a) -> m a
withLazyFile = withBinaryFileLazy


-- | Write a file in UTF8 encoding
--
-- This function will use OS-specific line ending handling.
writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeFileUtf8 fp text = liftIO $ withFile fp WriteMode $ \h -> do
  hSetEncoding h utf8
  T.hPutStr h text

-- | Write a `Builder` into a `Handle`. Lifted version of `BB.hPutBuilder`.
hPutBuilder :: MonadIO m => Handle -> Builder -> m ()
hPutBuilder h = liftIO . BB.hPutBuilder h
{-# INLINE hPutBuilder #-}

-- | Same as 'B.readFile', but generalized to 'MonadIO'
--
-- @since 0.1.10.0
readBinaryFile :: MonadIO m => FilePath -> m ByteString
readBinaryFile = liftIO . B.readFile

-- | Same as 'B.writeFile', but generalized to 'MonadIO'
--
-- @since 0.1.10.0
writeBinaryFile :: MonadIO m => FilePath -> ByteString -> m ()
writeBinaryFile fp = liftIO . B.writeFile fp

-- | Synonym for `readBinaryFile`.
readFileBinary :: MonadIO m => FilePath -> m ByteString
readFileBinary = readBinaryFile

-- | Synonym for `writeBinaryFile`.
writeFileBinary :: MonadIO m => FilePath -> ByteString -> m ()
writeFileBinary = writeBinaryFile

-- | Read a file in UTF8 encoding, throwing an exception on invalid character
-- encoding.
--
-- This function will use OS-specific line ending handling.
readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 fp = liftIO $ withFile fp ReadMode $ \h -> do
  hSetEncoding h utf8
  T.hGetContents h
