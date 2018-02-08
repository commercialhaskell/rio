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
  ) where

import RIO.Prelude.Reexports
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Vector.Generic      as GVector
import qualified Data.Vector.Storable     as SVector
import qualified Data.Vector.Unboxed      as UVector
import qualified Data.Text.Lazy           as TL
import qualified Data.Semigroup

sappend :: Semigroup s => s -> s -> s
sappend = (Data.Semigroup.<>)

type UVector = UVector.Vector
type SVector = SVector.Vector
type GVector = GVector.Vector

type LByteString = BL.ByteString
type LText = TL.Text

toStrictBytes :: LByteString -> ByteString
toStrictBytes = BL.toStrict

fromStrictBytes :: ByteString -> LByteString
fromStrictBytes = BL.fromStrict
