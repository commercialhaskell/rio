{-# LANGUAGE ConstraintKinds #-}
module RIO.Prelude.Renames
  ( sappend
  , LByteString
  , LText
  , UVector
  , SVector
  , GVector
  , toStrictBytes
  , fromStrictBytes
  , yieldThread
  ) where

import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Vector.Generic      as GVector
import qualified Data.Vector.Storable     as SVector
import qualified Data.Vector.Unboxed      as UVector
import qualified Data.Text.Lazy           as TL
import qualified Data.Semigroup
import UnliftIO (MonadIO)
import qualified UnliftIO.Concurrent (yield)

sappend :: Data.Semigroup.Semigroup s => s -> s -> s
sappend = (Data.Semigroup.<>)

type UVector = UVector.Vector
type SVector = SVector.Vector
type GVector = GVector.Vector

type LByteString = BL.ByteString
type LText = TL.Text

toStrictBytes :: LByteString -> B.ByteString
toStrictBytes = BL.toStrict

fromStrictBytes :: B.ByteString -> LByteString
fromStrictBytes = BL.fromStrict

yieldThread :: MonadIO m => m ()
yieldThread = UnliftIO.Concurrent.yield
{-# INLINE yieldThread #-}
