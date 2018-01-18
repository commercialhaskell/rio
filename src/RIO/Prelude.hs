{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module RIO.Prelude
  ( module UnliftIO
  , mapLeft
  , withLazyFile
  , fromFirst
  , mapMaybeA
  , mapMaybeM
  , forMaybeA
  , forMaybeM
  , stripCR
  , RIO (..)
  , runRIO
  , liftRIO
  , tshow
  , nubOrd
  , readFileBinary
  , writeFileBinary
  , ReadFileUtf8Exception (..)
  , readFileUtf8
  , writeFileUtf8
  , LByteString
  , toStrictBytes
  , fromStrictBytes
  , decodeUtf8Lenient
  , LText
  , view
  , UVector
  , SVector
  , GVector
  , DisplayBuilder (..)
  , Display (..)
  , displayShow
  , displayBuilderToText
  , displayBytesUtf8
  , writeFileDisplayBuilder
  , hPutBuilder
  , sappend
  , Control.Applicative.Alternative
  , Control.Applicative.Applicative (..)
  , Control.Applicative.liftA
#if !MIN_VERSION_base(4, 10, 0)
  , Control.Applicative.liftA2
#endif
  , Control.Applicative.liftA3
  , Control.Applicative.many
  , Control.Applicative.optional
  , Control.Applicative.some
  , (Control.Applicative.<|>)
  , Control.Arrow.first
  , Control.Arrow.second
  , (Control.Arrow.&&&)
  , (Control.Arrow.***)
  , Control.DeepSeq.NFData(..)
  , Control.DeepSeq.force
  , (Control.DeepSeq.$!!)
  , Control.Monad.Monad(..)
  , Control.Monad.MonadPlus(..)
  , Control.Monad.filterM
  , Control.Monad.foldM
  , Control.Monad.foldM_
  , Control.Monad.forever
  , Control.Monad.guard
  , Control.Monad.join
  , Control.Monad.liftM
  , Control.Monad.liftM2
  , Control.Monad.replicateM_
  , Control.Monad.unless
  , Control.Monad.when
  , Control.Monad.zipWithM
  , Control.Monad.zipWithM_
  , (Control.Monad.<$!>)
  , (Control.Monad.<=<)
  , (Control.Monad.=<<)
  , (Control.Monad.>=>)
  , Control.Monad.Catch.MonadThrow(..)
  , Control.Monad.Reader.MonadReader
  , Control.Monad.Reader.MonadTrans(..)
  , Control.Monad.Reader.ReaderT(..)
  , Control.Monad.Reader.ask
  , Control.Monad.Reader.asks
  , Control.Monad.Reader.local
  , Data.Bool.Bool(..)
  , Data.Bool.not
  , Data.Bool.otherwise
  , (Data.Bool.&&)
  , (Data.Bool.||)
  , Data.ByteString.ByteString
  , Data.ByteString.Builder.Builder
  , Data.ByteString.Short.ShortByteString
  , Data.ByteString.Short.toShort
  , Data.ByteString.Short.fromShort
  , Data.Char.Char
  , Data.Data.Data(..)
  , Data.Either.Either(..)
  , Data.Either.either
  , Data.Either.isLeft
  , Data.Either.isRight
  , Data.Either.lefts
  , Data.Either.partitionEithers
  , Data.Either.rights
  , Data.Eq.Eq(..)
  , Data.Foldable.Foldable
  , Data.Foldable.all
  , Data.Foldable.and
  , Data.Foldable.any
  , Data.Foldable.asum
  , Data.Foldable.concat
  , Data.Foldable.concatMap
  , Data.Foldable.elem
  , Data.Foldable.fold
  , Data.Foldable.foldMap
  , Data.Foldable.foldl'
  , Data.Foldable.foldr
  , Data.Foldable.forM_
  , Data.Foldable.for_
  , Data.Foldable.length
  , Data.Foldable.mapM_
  , Data.Foldable.msum
  , Data.Foldable.notElem
  , Data.Foldable.null
  , Data.Foldable.or
  , Data.Foldable.product
  , Data.Foldable.sequenceA_
  , Data.Foldable.sequence_
  , Data.Foldable.sum
  , Data.Foldable.toList
  , Data.Foldable.traverse_
  , Data.Function.const
  , Data.Function.fix
  , Data.Function.flip
  , Data.Function.id
  , Data.Function.on
  , (Data.Function.$)
  , (Data.Function.&)
  , (Data.Function..)
  , Data.Functor.Functor(..)
  , Data.Functor.void
  , (Data.Functor.$>)
  , (Data.Functor.<$>)
  , Data.Hashable.Hashable
  , Data.HashMap.Strict.HashMap
  , Data.HashSet.HashSet
  , Data.Int.Int
  , Data.Int.Int8
  , Data.Int.Int16
  , Data.Int.Int32
  , Data.Int.Int64
  , Data.IntMap.Strict.IntMap
  , Data.IntSet.IntSet
  , Data.List.break
  , Data.List.drop
  , Data.List.dropWhile
  , Data.List.filter
  , Data.List.lines
  , Data.List.lookup
  , Data.List.map
  , Data.List.replicate
  , Data.List.reverse
  , Data.List.span
  , Data.List.take
  , Data.List.takeWhile
  , Data.List.unlines
  , Data.List.unwords
  , Data.List.words
  , Data.List.zip
  , (Data.List.++)
  , Data.Map.Strict.Map
  , Data.Maybe.Maybe(..)
  , Data.Maybe.catMaybes
  , Data.Maybe.fromMaybe
  , Data.Maybe.isJust
  , Data.Maybe.isNothing
  , Data.Maybe.listToMaybe
  , Data.Maybe.mapMaybe
  , Data.Maybe.maybe
  , Data.Maybe.maybeToList
  , Data.Monoid.All (..)
  , Data.Monoid.Any (..)
  , Data.Monoid.Endo (..)
  , Data.Monoid.First (..)
  , Data.Monoid.Last (..)
  , Data.Monoid.Monoid (..)
  , Data.Monoid.Product (..)
  , Data.Monoid.Sum (..)
  , (Data.Monoid.<>)
  , Data.Ord.Ord(..)
  , Data.Ord.Ordering(..)
  , Data.Ord.comparing
  , Data.Semigroup.Semigroup
  , Data.Set.Set
  , Data.String.IsString(..)
  , Data.Text.Text
  , Data.Text.Encoding.decodeUtf8'
  , Data.Text.Encoding.decodeUtf8With
  , Data.Text.Encoding.encodeUtf8
  , Data.Text.Encoding.encodeUtf8Builder
  , Data.Text.Encoding.Error.UnicodeException(..)
  , Data.Text.Encoding.Error.lenientDecode
  , Data.Traversable.Traversable(..)
  , Data.Traversable.for
  , Data.Traversable.forM
  , Data.Vector.Vector
  , Data.Void.Void
  , Data.Void.absurd
  , Data.Word.Word
  , Data.Word.Word8
  , Data.Word.Word16
  , Data.Word.Word32
  , Data.Word.Word64
  , Data.Word.byteSwap16
  , Data.Word.byteSwap32
  , Data.Word.byteSwap64
  , Foreign.Storable.Storable
  , GHC.Generics.Generic
  , GHC.Stack.HasCallStack
  , Lens.Micro.ASetter
  , Lens.Micro.ASetter'
  , Lens.Micro.Getting
  , Lens.Micro.Lens
  , Lens.Micro.Lens'
  , Lens.Micro.SimpleGetter
  , Lens.Micro.lens
  , Lens.Micro.over
  , Lens.Micro.set
  , Lens.Micro.sets
  , Lens.Micro.to
  , (Lens.Micro.^.)
  , Prelude.Bounded (..)
  , Prelude.Double
  , Prelude.Enum
  , Prelude.FilePath
  , Prelude.Float
  , Prelude.Floating (..)
  , Prelude.Fractional (..)
  , Prelude.IO
  , Prelude.Integer
  , Prelude.Integral (..)
  , Prelude.Num (..)
  , Prelude.Rational
  , Prelude.Real (..)
  , Prelude.RealFloat (..)
  , Prelude.RealFrac (..)
  , Prelude.Show
  , Prelude.String
  , Prelude.asTypeOf
  , Prelude.curry
  , Prelude.error
  , Prelude.even
  , Prelude.fromIntegral
  , Prelude.fst
  , Prelude.gcd
  , Prelude.lcm
  , Prelude.odd
  , Prelude.realToFrac
  , Prelude.seq
  , Prelude.show
  , Prelude.snd
  , Prelude.subtract
  , Prelude.uncurry
  , Prelude.undefined
  , (Prelude.$!)
  , (Prelude.^)
  , (Prelude.^^)
  , System.Exit.ExitCode(..)
  , Text.Read.Read
  , Text.Read.readMaybe
  -- List imports from UnliftIO?
  ) where

-- Fixed imports go here
import           Control.Applicative      (Applicative)
import           Control.Monad            (Monad (..), liftM, (<=<))
import           Control.Monad.Catch      (MonadThrow)
import           Control.Monad.Reader     (MonadReader, ReaderT (..), ask, asks)
import           Data.Bool                (otherwise)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Builder  (Builder)
import           Data.Either              (Either (..))
import           Data.Foldable            (foldMap)
import           Data.Function            (flip, ($), (.))
import           Data.Functor             (Functor (..))
import           Data.Int                 (Int)
import           Data.Maybe               (Maybe, catMaybes, fromMaybe)
import           Data.Monoid              (First (..), Monoid (..))
import           Data.Ord                 (Ord)
import           Data.Semigroup           (Semigroup)
import           Data.String              (IsString (..))
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8', decodeUtf8With,
                                           encodeUtf8, encodeUtf8Builder)
import           Data.Text.Encoding.Error (UnicodeException, lenientDecode)
import           Data.Traversable         (Traversable (..))
import           Lens.Micro               (Getting)
import           Prelude                  (FilePath, IO, Show (..))
import           UnliftIO
-- import           UnliftIO                 (Exception, Handle, IOMode (..),
--                                            MonadIO (..), MonadUnliftIO,
--                                            Typeable, UnliftIO (..), throwIO,
--                                            withBinaryFile, withUnliftIO)

import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL

import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL

import qualified Data.Vector.Generic      as GVector
import qualified Data.Vector.Storable     as SVector
import qualified Data.Vector.Unboxed      as UVector

import qualified Data.ByteString.Builder  as BB
import qualified Data.Semigroup

import           Control.Applicative      (Const (..))
import           Lens.Micro.Internal      (( #. ))

import qualified Data.Set                 as Set

-- Reexports
import qualified Control.Applicative
import qualified Control.Arrow
import qualified Control.DeepSeq
import qualified Control.Monad
import qualified Control.Monad.Catch
import qualified Control.Monad.Reader
import qualified Data.Bool
import qualified Data.ByteString
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Short
import qualified Data.Char
import qualified Data.Data
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Foldable
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Hashable
import qualified Data.HashMap.Strict
import qualified Data.HashSet
import qualified Data.Int
import qualified Data.IntMap.Strict
import qualified Data.IntSet
import qualified Data.List
import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Ord
import qualified Data.Semigroup
import qualified Data.Set
import qualified Data.String
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified Data.Traversable
import qualified Data.Vector
import qualified Data.Void
import qualified Data.Word
import qualified Foreign.Storable
import qualified GHC.Generics
import qualified GHC.Stack
import qualified Lens.Micro
import qualified Prelude
import qualified System.Exit
import qualified Text.Read
import qualified UnliftIO


mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f (Left a1) = Left (f a1)
mapLeft _ (Right b) = Right b

fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst

-- | Applicative 'mapMaybe'.
mapMaybeA :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
mapMaybeA f = fmap catMaybes . traverse f

-- | @'forMaybeA' '==' 'flip' 'mapMaybeA'@
forMaybeA :: Applicative f => [a] -> (a -> f (Maybe b)) -> f [b]
forMaybeA = flip mapMaybeA

-- | Monadic 'mapMaybe'.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f

-- | @'forMaybeM' '==' 'flip' 'mapMaybeM'@
forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM

-- | Strip trailing carriage return from Text
stripCR :: T.Text -> T.Text
stripCR t = fromMaybe t (T.stripSuffix "\r" t)

-- | Lazily get the contents of a file. Unlike 'BL.readFile', this
-- ensures that if an exception is thrown, the file handle is closed
-- immediately.
withLazyFile :: MonadUnliftIO m => FilePath -> (BL.ByteString -> m a) -> m a
withLazyFile fp inner = withBinaryFile fp ReadMode $ inner <=< liftIO . BL.hGetContents

-- | The Reader+IO monad. This is different from a 'ReaderT' because:
--
-- * It's not a transformer, it hardcodes IO for simpler usage and
-- error messages.
--
-- * Instances of typeclasses like 'MonadLogger' are implemented using
-- classes defined on the environment, instead of using an
-- underlying monad.
newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader env,MonadThrow)

runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO (f env)

liftRIO :: (MonadIO m, MonadReader env m) => RIO env a -> m a
liftRIO rio = do
  env <- ask
  runRIO env rio

instance MonadUnliftIO (RIO env) where
    askUnliftIO = RIO $ ReaderT $ \r ->
                  withUnliftIO $ \u ->
                  return (UnliftIO (unliftIO u . flip runReaderT r . unRIO))

tshow :: Show a => a -> Text
tshow = T.pack . show

nubOrd :: Ord a => [a] -> [a]
nubOrd =
  loop mempty
  where
    loop _ [] = []
    loop !s (a:as)
      | a `Set.member` s = loop s as
      | otherwise = a : loop (Set.insert a s) as

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

data ReadFileUtf8Exception = ReadFileUtf8Exception !FilePath !UnicodeException
  deriving (Show, Typeable)
instance Exception ReadFileUtf8Exception

-- | Write a file in UTF8 encoding
writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeFileUtf8 fp = writeFileBinary fp . encodeUtf8

type LByteString = BL.ByteString

toStrictBytes :: LByteString -> ByteString
toStrictBytes = BL.toStrict

fromStrictBytes :: ByteString -> LByteString
fromStrictBytes = BL.fromStrict

view :: MonadReader s m => Getting a s a -> m a
view l = asks (getConst #. l Const)

type UVector = UVector.Vector
type SVector = SVector.Vector
type GVector = GVector.Vector

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

type LText = TL.Text

newtype DisplayBuilder = DisplayBuilder { getUtf8Builder :: Builder }
  deriving (Semigroup, Monoid)

instance IsString DisplayBuilder where
  fromString = DisplayBuilder . BB.stringUtf8

class Display a where
  display :: a -> DisplayBuilder
instance Display Text where
  display = DisplayBuilder . encodeUtf8Builder
instance Display LText where
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

sappend :: Semigroup s => s -> s -> s
sappend = (Data.Semigroup.<>)

writeFileDisplayBuilder :: MonadIO m => FilePath -> DisplayBuilder -> m ()
writeFileDisplayBuilder fp (DisplayBuilder builder) =
  liftIO $ withBinaryFile fp WriteMode $ \h -> hPutBuilder h builder

hPutBuilder :: MonadIO m => Handle -> Builder -> m ()
hPutBuilder h = liftIO . BB.hPutBuilder h
{-# INLINE hPutBuilder #-}
