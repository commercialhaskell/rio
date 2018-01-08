-- | Strict @ByteString@. Import as:
--
-- > import qualified RIO.ByteString as B
{-# LANGUAGE NoImplicitPrelude #-}
module RIO.ByteString
  ( module Data.ByteString
  , module RIO.ByteString
  ) where

import Data.ByteString hiding (head, last, tail, init, foldl1, foldl1', foldr1, foldr1', maximum, minimum, findSubstring, findSubstrings, packCString, packCStringLen, useAsCString, useAsCStringLen, getLine, getContents, putStr, putStrLn, interact, readFile, writeFile, appendFile, hGetLine, hGetContents, hGet, hGetSome, hGetNonBlocking, hPut, hPutNonBlocking, hPutStr, hPutStrLn, breakByte)
import qualified Data.ByteString as B
import RIO.Prelude
import Foreign.C.String (CString, CStringLen)

-- | Lifted 'B.packCString'
packCString :: MonadIO m => CString -> m ByteString
packCString = liftIO . B.packCString

-- | Lifted 'B.packCStringLen'
packCStringLen :: MonadIO m => CStringLen -> m ByteString
packCStringLen = liftIO . B.packCStringLen

-- | Unlifted 'B.useAsCString'
useAsCString :: MonadUnliftIO m => ByteString -> (CString -> m a) -> m a
useAsCString bs inner = withRunInIO $ \run -> B.useAsCString bs $ run . inner

-- | Unlifted 'B.useAsCStringLen'
useAsCStringLen :: MonadUnliftIO m => ByteString -> (CStringLen -> m a) -> m a
useAsCStringLen bs inner = withRunInIO $ \run -> B.useAsCStringLen bs $ run . inner

-- | Lifted 'B.getLine'
getLine :: MonadIO m => m ByteString
getLine = liftIO B.getLine

-- | Lifted 'B.getContents'
getContents :: MonadIO m => m ByteString
getContents = liftIO B.getContents

-- | Lifted 'B.putStr'
putStr :: MonadIO m => ByteString -> m ()
putStr = liftIO . B.putStr

-- | Lifted 'B.interact'
interact :: MonadIO m => (ByteString -> ByteString) -> m ()
interact = liftIO . B.interact

-- | Lifted 'B.readFile'
readFile :: MonadIO m => FilePath -> m ByteString
readFile = liftIO . B.readFile

-- | Lifted 'B.writeFile'
writeFile :: MonadIO m => FilePath -> ByteString -> m ()
writeFile fp = liftIO . B.writeFile fp

-- | Lifted 'B.appendFile'
appendFile :: MonadIO m => FilePath -> ByteString -> m ()
appendFile fp = liftIO . B.appendFile fp

-- | Lifted 'B.hGetLine'
hGetLine :: MonadIO m => Handle -> m ByteString
hGetLine = liftIO . B.hGetLine

-- | Lifted 'B.hGetContents'
hGetContents :: MonadIO m => Handle -> m ByteString
hGetContents = liftIO . B.hGetContents

-- | Lifted 'B.hGet'
hGet :: MonadIO m => Handle -> Int -> m ByteString
hGet h = liftIO . B.hGet h

-- | Lifted 'B.hGetSome'
hGetSome :: MonadIO m => Handle -> Int -> m ByteString
hGetSome h = liftIO . B.hGetSome h

-- | Lifted 'B.hGetNonBlocking'
hGetNonBlocking :: MonadIO m => Handle -> Int -> m ByteString
hGetNonBlocking h = liftIO . B.hGetNonBlocking h

-- | Lifted 'B.hPut'
hPut :: MonadIO m => Handle -> ByteString -> m ()
hPut h = liftIO . B.hPut h

-- | Lifted 'B.hPutNonBlocking'
hPutNonBlocking :: MonadIO m => Handle -> ByteString -> m ByteString
hPutNonBlocking h = liftIO . B.hPutNonBlocking h

-- | Lifted 'B.hPutStr'
hPutStr :: MonadIO m => Handle -> ByteString -> m ()
hPutStr h = liftIO . B.hPutStr h
