-- | Lazy @Text@. Import as:
--
-- > import qualified RIO.Text.Lazy as TL
--
-- This module does not export any partial functions.  For those, see
-- "RIO.Text.Lazy.Partial"
module RIO.Text.Lazy
    (
    -- * Types
      Data.Text.Lazy.Text

    -- * Creation and elimination
    , Data.Text.Lazy.pack
    , Data.Text.Lazy.unpack
    , Data.Text.Lazy.singleton
    , Data.Text.Lazy.empty
    , Data.Text.Lazy.fromChunks
    , Data.Text.Lazy.toChunks
    , Data.Text.Lazy.toStrict
    , Data.Text.Lazy.fromStrict
    , Data.Text.Lazy.foldrChunks
    , Data.Text.Lazy.foldlChunks

    -- * Basic interface
    , Data.Text.Lazy.cons
    , Data.Text.Lazy.snoc
    , Data.Text.Lazy.append
    , Data.Text.Lazy.uncons
    , Data.Text.Lazy.null
    , Data.Text.Lazy.length
    , Data.Text.Lazy.compareLength

    -- * Transformations
    , Data.Text.Lazy.map
    , Data.Text.Lazy.intercalate
    , Data.Text.Lazy.intersperse
    , Data.Text.Lazy.transpose
    , Data.Text.Lazy.reverse
    , Data.Text.Lazy.replace

    -- ** Case conversion
    , Data.Text.Lazy.toCaseFold
    , Data.Text.Lazy.toLower
    , Data.Text.Lazy.toUpper
    , Data.Text.Lazy.toTitle

    -- ** Justification
    , Data.Text.Lazy.justifyLeft
    , Data.Text.Lazy.justifyRight
    , Data.Text.Lazy.center

    -- * Folds
    , Data.Text.Lazy.foldl
    , Data.Text.Lazy.foldl'
    , Data.Text.Lazy.foldr

    -- ** Special folds
    , Data.Text.Lazy.concat
    , Data.Text.Lazy.concatMap
    , Data.Text.Lazy.any
    , Data.Text.Lazy.all

    -- * Construction

    -- ** Scans
    , Data.Text.Lazy.scanl
    , Data.Text.Lazy.scanl1  -- NB. scanl1 and scanr1 are not partial
    , Data.Text.Lazy.scanr
    , Data.Text.Lazy.scanr1

    -- ** Accumulating maps
    , Data.Text.Lazy.mapAccumL
    , Data.Text.Lazy.mapAccumR

    -- ** Generation and unfolding
    , Data.Text.Lazy.repeat
    , Data.Text.Lazy.replicate
    , Data.Text.Lazy.cycle
    , Data.Text.Lazy.iterate
    , Data.Text.Lazy.unfoldr
    , Data.Text.Lazy.unfoldrN

    -- * Substrings

    -- ** Breaking strings
    , Data.Text.Lazy.take
    , Data.Text.Lazy.takeEnd
    , Data.Text.Lazy.drop
    , Data.Text.Lazy.dropEnd
    , Data.Text.Lazy.takeWhile
    , Data.Text.Lazy.takeWhileEnd
    , Data.Text.Lazy.dropWhile
    , Data.Text.Lazy.dropWhileEnd
    , Data.Text.Lazy.dropAround
    , Data.Text.Lazy.strip
    , Data.Text.Lazy.stripStart
    , Data.Text.Lazy.stripEnd
    , Data.Text.Lazy.splitAt
    , Data.Text.Lazy.span
    , Data.Text.Lazy.break
    , Data.Text.Lazy.group
    , Data.Text.Lazy.groupBy
    , Data.Text.Lazy.inits
    , Data.Text.Lazy.tails

    -- ** Breaking into many substrings
    , Data.Text.Lazy.split
    , Data.Text.Lazy.chunksOf

    -- ** Breaking into lines and words
    , Data.Text.Lazy.lines
    , Data.Text.Lazy.words
    , Data.Text.Lazy.unlines
    , Data.Text.Lazy.unwords

    -- * Predicates
    , Data.Text.Lazy.isPrefixOf
    , Data.Text.Lazy.isSuffixOf
    , Data.Text.Lazy.isInfixOf

    -- ** View patterns
    , Data.Text.Lazy.stripPrefix
    , Data.Text.Lazy.stripSuffix
    , Data.Text.Lazy.commonPrefixes

    -- * Searching
    , Data.Text.Lazy.filter
    , Data.Text.Lazy.find
    , Data.Text.Lazy.partition

    -- * Indexing
    , Data.Text.Lazy.index
    , Data.Text.Lazy.count

    -- * Zipping and unzipping
    , Data.Text.Lazy.zip
    , Data.Text.Lazy.zipWith
    ) where

import qualified Data.Text.Lazy
