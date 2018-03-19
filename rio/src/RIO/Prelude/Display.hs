{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RIO.Prelude.Display
  ( Utf8Builder (..)
  , Display (..)
  , displayShow
  , utf8BuilderToText
  , utf8BuilderToLazyText
  , displayBytesUtf8
  , writeFileUtf8Builder
  ) where

import Data.String (IsString (..))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Builder  as BB
import           Data.ByteString.Builder  (Builder)
import           Data.Semigroup           (Semigroup)
import           Data.Text                (Text)
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import UnliftIO
import           Data.Text.Encoding       (decodeUtf8With, encodeUtf8Builder)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Int
import           Data.Word

-- | A builder of binary data, with the invariant that the underlying
-- data is supposed to be UTF-8 encoded.
--
-- @since 0.1.0.0
newtype Utf8Builder = Utf8Builder { getUtf8Builder :: Builder }
  deriving (Semigroup, Monoid)

-- | @since 0.1.0.0
instance IsString Utf8Builder where
  fromString = Utf8Builder . BB.stringUtf8

-- | A typeclass for values which can be converted to a
-- 'Utf8Builder'. The intention of this typeclass is to provide a
-- human-friendly display of the data.
--
-- @since 0.1.0.0
class Display a where
  display :: a -> Utf8Builder

-- | @since 0.1.0.0
instance Display Utf8Builder where
  display = id
-- | @since 0.1.0.0
instance Display Text where
  display = Utf8Builder . encodeUtf8Builder
-- | @since 0.1.0.0
instance Display TL.Text where
  display = foldMap display . TL.toChunks
-- | @since 0.1.0.0
instance Display Char where
  display = Utf8Builder . BB.charUtf8

-- | @since 0.1.0.0
instance Display Integer where
  display = Utf8Builder . BB.integerDec
-- | @since 0.1.0.0
instance Display Float where
  display = Utf8Builder . BB.floatDec
instance Display Double where
  display = Utf8Builder . BB.doubleDec

-- | @since 0.1.0.0
instance Display Int where
  display = Utf8Builder . BB.intDec
-- | @since 0.1.0.0
instance Display Int8 where
  display = Utf8Builder . BB.int8Dec
-- | @since 0.1.0.0
instance Display Int16 where
  display = Utf8Builder . BB.int16Dec
-- | @since 0.1.0.0
instance Display Int32 where
  display = Utf8Builder . BB.int32Dec
-- | @since 0.1.0.0
instance Display Int64 where
  display = Utf8Builder . BB.int64Dec

-- | @since 0.1.0.0
instance Display Word where
  display = Utf8Builder . BB.wordDec
-- | @since 0.1.0.0
instance Display Word8 where
  display = Utf8Builder . BB.word8Dec
-- | @since 0.1.0.0
instance Display Word16 where
  display = Utf8Builder . BB.word16Dec
-- | @since 0.1.0.0
instance Display Word32 where
  display = Utf8Builder . BB.word32Dec
-- | @since 0.1.0.0
instance Display Word64 where
  display = Utf8Builder . BB.word64Dec

-- | Use the 'Show' instance for a value to convert it to a
-- 'Utf8Builder'.
--
-- @since 0.1.0.0
displayShow :: Show a => a -> Utf8Builder
displayShow = fromString . show

-- | Convert a 'ByteString' into a 'Utf8Builder'.
--
-- /NOTE/ This function performs no checks to ensure that the data is,
-- in fact, UTF8 encoded. If you provide non-UTF8 data, later
-- functions may fail.
--
-- @since 0.1.0.0
displayBytesUtf8 :: ByteString -> Utf8Builder
displayBytesUtf8 = Utf8Builder . BB.byteString

-- | Convert a 'Utf8Builder' value into a strict 'Text'.
--
-- @since 0.1.0.0
utf8BuilderToText :: Utf8Builder -> Text
utf8BuilderToText =
  decodeUtf8With lenientDecode . BL.toStrict . BB.toLazyByteString . getUtf8Builder

-- | Convert a 'Utf8Builder' value into a lazy 'Text'.
--
-- @since 0.1.0.0
utf8BuilderToLazyText :: Utf8Builder -> TL.Text
utf8BuilderToLazyText =
  TL.decodeUtf8With lenientDecode . BB.toLazyByteString . getUtf8Builder

-- | Write the given 'Utf8Builder' value to a file.
--
-- @since 0.1.0.0
writeFileUtf8Builder :: MonadIO m => FilePath -> Utf8Builder -> m ()
writeFileUtf8Builder fp (Utf8Builder builder) =
  liftIO $ withBinaryFile fp WriteMode $ \h -> BB.hPutBuilder h builder
