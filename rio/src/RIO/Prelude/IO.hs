module RIO.Prelude.IO
  ( withLazyFile
  , readFileBinary
  , writeFileBinary
  , readFileUtf8
  , writeFileUtf8
  , hPutBuilder
  ) where

import RIO.Prelude.Reexports
import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text.IO             as T
import           System.IO                (hSetEncoding, utf8)

-- | Lazily get the contents of a file. Unlike 'BL.readFile', this
-- ensures that if an exception is thrown, the file handle is closed
-- immediately.
withLazyFile :: MonadUnliftIO m => FilePath -> (BL.ByteString -> m a) -> m a
withLazyFile fp inner = withBinaryFile fp ReadMode $ inner <=< liftIO . BL.hGetContents

-- | Write a file in UTF8 encoding
--
-- This function will use OS-specific line ending handling.
writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeFileUtf8 fp text = liftIO $ withFile fp WriteMode $ \h -> do
  hSetEncoding h utf8
  T.hPutStr h text

hPutBuilder :: MonadIO m => Handle -> Builder -> m ()
hPutBuilder h = liftIO . BB.hPutBuilder h
{-# INLINE hPutBuilder #-}

-- | Same as 'B.readFile', but generalized to 'MonadIO'
readFileBinary :: MonadIO m => FilePath -> m ByteString
readFileBinary = liftIO . B.readFile

-- | Same as 'B.writeFile', but generalized to 'MonadIO'
writeFileBinary :: MonadIO m => FilePath -> ByteString -> m ()
writeFileBinary fp = liftIO . B.writeFile fp

-- | Read a file in UTF8 encoding, throwing an exception on invalid character
-- encoding.
--
-- This function will use OS-specific line ending handling.
readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 fp = liftIO $ withFile fp ReadMode $ \h -> do
  hSetEncoding h utf8
  T.hGetContents h
