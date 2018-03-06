-- | Lazy @ByteString@. Import as:
--
-- > import qualified RIO.ByteString.Lazy as B.Lazy
module RIO.ByteString.Lazy
  (
  -- * The @ByteString@ type
    Data.ByteString.Lazy.ByteString

  -- * Introducing and eliminating 'ByteString's
  , Data.ByteString.Lazy.empty
  , Data.ByteString.Lazy.singleton
  , Data.ByteString.Lazy.pack
  , Data.ByteString.Lazy.unpack
  , Data.ByteString.Lazy.fromStrict
  , Data.ByteString.Lazy.toStrict
  , Data.ByteString.Lazy.fromChunks
  , Data.ByteString.Lazy.toChunks
  , Data.ByteString.Lazy.foldrChunks
  , Data.ByteString.Lazy.foldlChunks

  -- * Basic interface
  , Data.ByteString.Lazy.cons
  , Data.ByteString.Lazy.cons'
  , Data.ByteString.Lazy.snoc
  , Data.ByteString.Lazy.append
  , Data.ByteString.Lazy.uncons
  , Data.ByteString.Lazy.unsnoc
  , Data.ByteString.Lazy.null
  , Data.ByteString.Lazy.length

  -- * Transforming ByteStrings
  , Data.ByteString.Lazy.map
  , Data.ByteString.Lazy.reverse
  , Data.ByteString.Lazy.intersperse
  , Data.ByteString.Lazy.intercalate
  , Data.ByteString.Lazy.transpose

  -- * Reducing 'ByteString's (folds)
  , Data.ByteString.Lazy.foldl
  , Data.ByteString.Lazy.foldl'
  , Data.ByteString.Lazy.foldr

  -- ** Special folds
  , Data.ByteString.Lazy.concat
  , Data.ByteString.Lazy.concatMap
  , Data.ByteString.Lazy.any
  , Data.ByteString.Lazy.all

  -- * Building ByteStrings
  -- ** Scans
  , Data.ByteString.Lazy.scanl

  -- ** Accumulating maps
  , Data.ByteString.Lazy.mapAccumL
  , Data.ByteString.Lazy.mapAccumR

  -- ** Infinite ByteStrings
  , Data.ByteString.Lazy.repeat
  , Data.ByteString.Lazy.replicate
  , Data.ByteString.Lazy.cycle
  , Data.ByteString.Lazy.iterate

  -- ** Unfolding ByteStrings
  , Data.ByteString.Lazy.unfoldr

  -- * Substrings
  -- ** Breaking strings
  , Data.ByteString.Lazy.take
  , Data.ByteString.Lazy.drop
  , Data.ByteString.Lazy.splitAt
  , Data.ByteString.Lazy.takeWhile
  , Data.ByteString.Lazy.dropWhile
  , Data.ByteString.Lazy.span
  , Data.ByteString.Lazy.break
  , Data.ByteString.Lazy.group
  , Data.ByteString.Lazy.groupBy
  , Data.ByteString.Lazy.inits
  , Data.ByteString.Lazy.tails
  , Data.ByteString.Lazy.stripPrefix
  , Data.ByteString.Lazy.stripSuffix

  -- ** Breaking into many substrings
  , Data.ByteString.Lazy.split
  , Data.ByteString.Lazy.splitWith

  -- * Predicates
  , Data.ByteString.Lazy.isPrefixOf
  , Data.ByteString.Lazy.isSuffixOf

  -- * Search ByteStrings
  -- ** Searching by equality
  , Data.ByteString.Lazy.elem
  , Data.ByteString.Lazy.notElem

  -- ** Searching with a predicate
  , Data.ByteString.Lazy.find
  , Data.ByteString.Lazy.filter
  , Data.ByteString.Lazy.partition

  -- * Indexing ByteStrings
  , Data.ByteString.Lazy.index
  , Data.ByteString.Lazy.elemIndex
  , Data.ByteString.Lazy.elemIndexEnd
  , Data.ByteString.Lazy.elemIndices
  , Data.ByteString.Lazy.findIndex
  , Data.ByteString.Lazy.findIndices
  , Data.ByteString.Lazy.count

  -- * Zipping and unzipping ByteStrings
  , Data.ByteString.Lazy.zip
  , Data.ByteString.Lazy.zipWith
  , Data.ByteString.Lazy.unzip

  -- * Low level conversions
  -- ** Copying ByteStrings
  , Data.ByteString.Lazy.copy

  -- * I\/O with 'ByteString's
  -- ** Standard input and output
  , Data.ByteString.Lazy.getContents
  , Data.ByteString.Lazy.putStr
  , Data.ByteString.Lazy.putStrLn
  , Data.ByteString.Lazy.interact

  -- ** Files
  , Data.ByteString.Lazy.readFile
  , Data.ByteString.Lazy.writeFile
  , Data.ByteString.Lazy.appendFile

  -- ** I\/O with Handles
  , Data.ByteString.Lazy.hGetContents
  , Data.ByteString.Lazy.hGet
  , Data.ByteString.Lazy.hGetNonBlocking
  , Data.ByteString.Lazy.hPut
  , Data.ByteString.Lazy.hPutNonBlocking
  , Data.ByteString.Lazy.hPutStr
  ) where

-- TODO lifted versions of the above I/O functions

import qualified Data.ByteString.Lazy
