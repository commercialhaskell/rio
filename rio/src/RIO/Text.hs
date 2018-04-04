{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Strict @Text@. Import as:
--
-- > import qualified RIO.Text as Text
--
-- This module does not export any partial functions.  For those, see
-- "RIO.Text.Partial"
module RIO.Text
    (
    -- * Types
      Data.Text.Text

    -- * Creation and elimination
    , Data.Text.pack
    , Data.Text.unpack
    , Data.Text.singleton
    , Data.Text.empty

    -- * Basic interface
    , Data.Text.cons
    , Data.Text.snoc
    , Data.Text.append
    , Data.Text.uncons
    , Data.Text.null
    , Data.Text.length
    , Data.Text.compareLength

    -- * Transformations
    , Data.Text.map
    , Data.Text.intercalate
    , Data.Text.intersperse
    , Data.Text.transpose
    , Data.Text.reverse

    -- ** Case conversion
    , Data.Text.toCaseFold
    , Data.Text.toLower
    , Data.Text.toUpper
    , Data.Text.toTitle

    -- ** Justification
    , Data.Text.justifyLeft
    , Data.Text.justifyRight
    , Data.Text.center

    -- * Folds
    , Data.Text.foldl
    , Data.Text.foldl'
    , Data.Text.foldr

    -- ** Special folds
    , Data.Text.concat
    , Data.Text.concatMap
    , Data.Text.any
    , Data.Text.all

    -- * Construction

    -- ** Scans
    , Data.Text.scanl
    , Data.Text.scanl1 -- scanl1 and scanr1 are /not/ partial
    , Data.Text.scanr
    , Data.Text.scanr1

    -- ** Accumulating maps
    , Data.Text.mapAccumL
    , Data.Text.mapAccumR

    -- ** Generation and unfolding
    , Data.Text.replicate
    , Data.Text.unfoldr
    , Data.Text.unfoldrN

    -- * Substrings

    -- ** Breaking strings
    , Data.Text.take
    , Data.Text.takeEnd
    , Data.Text.drop
    , Data.Text.dropEnd
    , Data.Text.takeWhile
    , Data.Text.takeWhileEnd
    , Data.Text.dropWhile
    , Data.Text.dropWhileEnd
    , Data.Text.dropAround
    , Data.Text.strip
    , Data.Text.stripStart
    , Data.Text.stripEnd
    , Data.Text.splitAt
    , Data.Text.break
    , Data.Text.span
    , Data.Text.group
    , Data.Text.groupBy
    , Data.Text.inits
    , Data.Text.tails

    -- ** Breaking into many substrings
    , Data.Text.split
    , Data.Text.chunksOf

    -- ** Breaking into lines and words
    , Data.Text.lines
    , linesCR
    , Data.Text.words
    , Data.Text.unlines
    , Data.Text.unwords

    -- * Predicates
    , Data.Text.isPrefixOf
    , Data.Text.isSuffixOf
    , Data.Text.isInfixOf

    -- ** View patterns
    , Data.Text.stripPrefix
    , Data.Text.stripSuffix
    , dropPrefix
    , dropSuffix
    , Data.Text.commonPrefixes

    -- * Searching
    , Data.Text.filter
    , Data.Text.find
    , Data.Text.partition

    -- * Indexing
    , Data.Text.index
    , Data.Text.findIndex

    -- * Zipping
    , Data.Text.zip
    , Data.Text.zipWith

    -- * Low level operations
    , Data.Text.copy
    , Data.Text.unpackCString#

    -- * Encoding
    , Data.Text.Encoding.encodeUtf8
    , Data.Text.Encoding.decodeUtf8With
    , Data.Text.Encoding.decodeUtf8'
    , Data.Text.Encoding.Error.lenientDecode
    ) where

import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text, stripPrefix, stripSuffix)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error

-- | Drop prefix if present, otherwise return original 'Text'.
--
-- @since 0.0.0.0
dropPrefix :: Text -- ^ prefix
           -> Text
           -> Text
dropPrefix prefix t = fromMaybe t (stripPrefix prefix t)

-- | Drop prefix if present, otherwise return original 'Text'.
--
-- @since 0.0.0.0
dropSuffix :: Text -- ^ suffix
           -> Text
           -> Text
dropSuffix suffix t = fromMaybe t (stripSuffix suffix t)

-- | 'linesCR' breaks a 'Text' up into a list of `Text`s at newline
-- 'Char's. It is very similar to 'Data.Text.lines', but it also removes
-- any trailing @'\r'@ characters. The resulting 'Text' values do not
-- contain newlines or trailing @'\r'@ characters.
--
-- @since 0.1.0.0
linesCR :: Text -> [Text]
linesCR = map (dropSuffix "\r") . Data.Text.lines
