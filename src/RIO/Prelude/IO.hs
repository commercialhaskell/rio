module RIO.Prelude.IO
  ( withLazyFile
  , readFileBinary
  , writeFileBinary
  , ReadFileUtf8Exception (..)
  , readFileUtf8
  , writeFileUtf8
  , hPutBuilder
  ) where

import RIO.Prelude.Reexports
import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL

-- | Lazily get the contents of a file. Unlike 'BL.readFile', this
-- ensures that if an exception is thrown, the file handle is closed
-- immediately.
withLazyFile :: MonadUnliftIO m => FilePath -> (BL.ByteString -> m a) -> m a
withLazyFile fp inner = withBinaryFile fp ReadMode $ inner <=< liftIO . BL.hGetContents

data ReadFileUtf8Exception = ReadFileUtf8Exception !FilePath !UnicodeException
  deriving (Show, Typeable)
instance Exception ReadFileUtf8Exception

-- | Write a file in UTF8 encoding
writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeFileUtf8 fp = writeFileBinary fp . encodeUtf8

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
readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 fp = do
  bs <- readFileBinary fp
  case decodeUtf8' bs of
    Left e     -> throwIO $ ReadFileUtf8Exception fp e
    Right text -> return text
