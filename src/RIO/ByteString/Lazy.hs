-- | Lazy @ByteString@. Import as:
--
-- > import qualified RIO.ByteString.Lazy as B.Lazy
{-# LANGUAGE NoImplicitPrelude #-}
module RIO.ByteString.Lazy
  (
  -- * The @ByteString@ type
    Data.ByteString.Lazy.ByteString

  -- * Introducing and eliminating 'ByteString's
  , B.Lazy.empty
  , B.Lazy.singleton
  , B.Lazy.pack
  , B.Lazy.unpack
  , B.Lazy.fromStrict
  , B.Lazy.toStrict
  , B.Lazy.fromChunks
  , B.Lazy.toChunks
  , B.Lazy.foldrChunks
  , B.Lazy.foldlChunks

  -- * Basic interface
  , B.Lazy.cons
  , B.Lazy.cons'
  , B.Lazy.snoc
  , B.Lazy.append
  , B.Lazy.uncons
  , B.Lazy.unsnoc
  , B.Lazy.null
  , B.Lazy.length

  -- * Transforming ByteStrings
  , B.Lazy.map
  , B.Lazy.reverse
  , B.Lazy.intersperse
  , B.Lazy.intercalate
  , B.Lazy.transpose

  -- * Reducing 'ByteString's (folds)
  , B.Lazy.foldl
  , B.Lazy.foldl'
  , B.Lazy.foldr

  -- ** Special folds
  , B.Lazy.concat
  , B.Lazy.concatMap
  , B.Lazy.any
  , B.Lazy.all

  -- * Building ByteStrings
  -- ** Scans
  , B.Lazy.scanl

  -- ** Accumulating maps
  , B.Lazy.mapAccumL
  , B.Lazy.mapAccumR

  -- ** Infinite ByteStrings
  , B.Lazy.repeat
  , B.Lazy.replicate
  , B.Lazy.cycle
  , B.Lazy.iterate

  -- ** Unfolding ByteStrings
  , B.Lazy.unfoldr

  -- * Substrings
  -- ** Breaking strings
  , B.Lazy.take
  , B.Lazy.drop
  , B.Lazy.splitAt
  , B.Lazy.takeWhile
  , B.Lazy.dropWhile
  , B.Lazy.span
  , B.Lazy.break
  , B.Lazy.group
  , B.Lazy.groupBy
  , B.Lazy.inits
  , B.Lazy.tails
  , B.Lazy.stripPrefix
  , B.Lazy.stripSuffix

  -- ** Breaking into many substrings
  , B.Lazy.split
  , B.Lazy.splitWith

  -- * Predicates
  , B.Lazy.isPrefixOf
  , B.Lazy.isSuffixOf

  -- * Search ByteStrings
  -- ** Searching by equality
  , B.Lazy.elem
  , B.Lazy.notElem

  -- ** Searching with a predicate
  , B.Lazy.find
  , B.Lazy.filter
  , B.Lazy.partition

  -- * Indexing ByteStrings
  , B.Lazy.index
  , B.Lazy.elemIndex
  , B.Lazy.elemIndexEnd
  , B.Lazy.elemIndices
  , B.Lazy.findIndex
  , B.Lazy.findIndices
  , B.Lazy.count

  -- * Zipping and unzipping ByteStrings
  , B.Lazy.zip
  , B.Lazy.zipWith
  , B.Lazy.unzip

  -- * Low level conversions
  -- ** Copying ByteStrings
  , B.Lazy.copy

  -- * I\/O with 'ByteString's
  -- ** Standard input and output
  , getContents
  , putStr
  , putStrLn
  , interact

  -- ** Files
  , readFile
  , writeFile
  , appendFile

  -- ** I\/O with Handles
  , hGetContents
  , hGet
  , hGetNonBlocking
  , hPut
  , hPutNonBlocking
  , hPutStr
  ) where

import Data.ByteString.Lazy hiding
  (
    getContents
  , putStr
  , putStrLn
  , interact
  , readFile
  , writeFile
  , appendFile
  , hGetContents
  , hGet
  , hGetNonBlocking
  , hPut
  , hPutNonBlocking
  , hPutStr
  )
import qualified Data.ByteString.Lazy as B.Lazy
import qualified Data.ByteString.Lazy.Char8
import RIO

-- | Lifted 'B.Lazy.getContents'
getContents :: MonadIO m => m LByteString
getContents = liftIO B.Lazy.getContents

-- | Lifted 'B.Lazy.putStr'
putStr :: MonadIO m => LByteString -> m ()
putStr = liftIO . B.Lazy.putStr

-- | Lifted 'B.Lazy.putStrLn'
putStrLn :: MonadIO m => LByteString -> m ()
putStrLn = liftIO . Data.ByteString.Lazy.Char8.putStrLn

-- | Lifted 'B.Lazy.interact'
interact :: MonadIO m => (LByteString -> LByteString) -> m ()
interact = liftIO . B.Lazy.interact

-- | Lifted 'B.Lazy.readFile'
readFile :: MonadIO m => FilePath -> m LByteString
readFile = liftIO . B.Lazy.readFile

-- | Lifted 'B.Lazy.writeFile'
writeFile :: MonadIO m => FilePath -> LByteString -> m ()
writeFile fp contents =
  liftIO $ B.Lazy.writeFile fp contents

-- | Lifted 'B.Lazy.appendFile'
appendFile :: MonadIO m => FilePath -> LByteString -> m ()
appendFile fp = liftIO . B.Lazy.appendFile fp

-- | Lifted 'B.Lazy.hGet'
hGet :: MonadIO m => Handle -> Int -> m LByteString
hGet handle count = liftIO $ B.Lazy.hGet handle count

-- | Lifted 'B.Lazy.hGetContents'
hGetContents :: MonadIO m => Handle -> m LByteString
hGetContents = liftIO . B.Lazy.hGetContents

-- | Lifted 'B.Lazy.hGetNonBlocking'
hGetNonBlocking :: MonadIO m => Handle -> Int -> m LByteString
hGetNonBlocking h = liftIO . B.Lazy.hGetNonBlocking h

-- | Lifted 'B.Lazy.hPut'
hPut :: MonadIO m => Handle -> LByteString -> m ()
hPut h = liftIO . B.Lazy.hPut h

-- | Lifted 'B.Lazy.hPutNonBlocking'
hPutNonBlocking :: MonadIO m => Handle -> LByteString -> m LByteString
hPutNonBlocking h = liftIO . B.Lazy.hPutNonBlocking h

-- | Lifted 'B.Lazy.hPutStr'
hPutStr :: MonadIO m => Handle -> LByteString -> m ()
hPutStr h = liftIO . B.Lazy.hPutStr h
