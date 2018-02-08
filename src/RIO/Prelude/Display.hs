{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RIO.Prelude.Display
  ( DisplayBuilder (..)
  , Display (..)
  , displayShow
  , displayBuilderToText
  , displayBytesUtf8
  , writeFileDisplayBuilder
  ) where

import Data.String (IsString (..))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Builder  as BB
import           Data.ByteString.Builder  (Builder)
import           Data.Semigroup           (Semigroup)
import           Data.Text                (Text)
import qualified Data.Text.Lazy           as TL
import UnliftIO
import           Data.Text.Encoding       (decodeUtf8With, encodeUtf8Builder)
import           Data.Text.Encoding.Error (lenientDecode)

newtype DisplayBuilder = DisplayBuilder { getUtf8Builder :: Builder }
  deriving (Semigroup, Monoid)

instance IsString DisplayBuilder where
  fromString = DisplayBuilder . BB.stringUtf8

class Display a where
  display :: a -> DisplayBuilder
instance Display Text where
  display = DisplayBuilder . encodeUtf8Builder
instance Display TL.Text where
  display = foldMap display . TL.toChunks
instance Display Int where
  display = DisplayBuilder . BB.intDec

displayShow :: Show a => a -> DisplayBuilder
displayShow = fromString . show

displayBytesUtf8 :: ByteString -> DisplayBuilder
displayBytesUtf8 = DisplayBuilder . BB.byteString

displayBuilderToText :: DisplayBuilder -> Text
displayBuilderToText =
  decodeUtf8With lenientDecode . BL.toStrict . BB.toLazyByteString . getUtf8Builder

writeFileDisplayBuilder :: MonadIO m => FilePath -> DisplayBuilder -> m ()
writeFileDisplayBuilder fp (DisplayBuilder builder) =
  liftIO $ withBinaryFile fp WriteMode $ \h -> BB.hPutBuilder h builder
