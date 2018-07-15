{-# LANGUAGE CPP #-}
module RIO.Prelude.Reexports
  ( module UnliftIO
  -- List imports from UnliftIO?
  , UnliftIO.Concurrent.ThreadId
  , UnliftIO.Concurrent.myThreadId
  , UnliftIO.Concurrent.isCurrentThreadBound
  , UnliftIO.Concurrent.threadWaitRead
  , UnliftIO.Concurrent.threadWaitWrite
  , UnliftIO.Concurrent.threadDelay
  , yieldThread
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
  , Control.Monad.Reader.Reader
  , Control.Monad.Reader.ReaderT(..)
  , Control.Monad.Reader.ask
  , Control.Monad.Reader.asks
  , Control.Monad.Reader.local
  , Control.Monad.Reader.runReader
  , Data.Bool.Bool(..)
  , Data.Bool.bool
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
  , Data.Functor.Const.Const(..)
  , Data.Functor.Identity.Identity(..)
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
  , Data.Monoid.Monoid (..)
  , Data.Ord.Ord(..)
  , Data.Ord.Ordering(..)
  , Data.Ord.comparing
  , Data.Semigroup.Semigroup (..)
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
  , Numeric.Natural.Natural
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
    -- * Primitive
  , PrimMonad (..)
    -- * Unbox
  , Unbox
  ) where

import           Control.Applicative      (Applicative)
import           Control.Monad            (Monad (..), liftM, (<=<))
import           Control.Monad.Catch      (MonadThrow)
import           Control.Monad.Primitive  (PrimMonad (..))
import           Control.Monad.Reader     (MonadReader, ReaderT (..), ask, asks)
import           Control.Monad.State      (MonadState(..))
import           Control.Monad.Writer     (MonadWriter (..))
import           Data.Bool                (otherwise)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Builder  (Builder)
import           Data.Either              (Either (..))
import           Data.Foldable            (foldMap)
import           Data.Function            (flip, ($), (.))
import           Data.Functor             (Functor (..))
import           Data.Int                 (Int)
import           Data.Maybe               (Maybe, catMaybes, fromMaybe)
import           Data.Monoid              (Monoid (..))
import           Data.Ord                 (Ord)
import           Data.Semigroup           (Semigroup (..))
import           Data.String              (IsString (..))
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8', decodeUtf8With,
                                           encodeUtf8, encodeUtf8Builder)
import           Data.Text.Encoding.Error (UnicodeException, lenientDecode)
import           Data.Traversable         (Traversable (..))
import           Prelude                  (FilePath, IO, Show (..))
import           UnliftIO
import qualified UnliftIO.Concurrent

import           Data.Vector.Unboxed.Mutable (Unbox)




-- Reexports
import qualified Control.Applicative
import qualified Control.Arrow
import qualified Control.DeepSeq
import qualified Control.Monad
import qualified Control.Monad.Catch
import qualified Control.Monad.Reader
import qualified Data.Bool
import qualified Data.ByteString.Short
import qualified Data.Char
import qualified Data.Data
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Foldable
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Functor.Const
import qualified Data.Functor.Identity
import qualified Data.Hashable
import qualified Data.HashMap.Strict
import qualified Data.HashSet
import qualified Data.Int
import qualified Data.IntMap.Strict
import qualified Data.IntSet
import qualified Data.List
import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified Data.Ord
import qualified Data.Set
import qualified Data.Text.Encoding.Error
import qualified Data.Traversable
import qualified Data.Vector
import qualified Data.Void
import qualified Data.Word
import qualified Foreign.Storable
import qualified GHC.Generics
import qualified GHC.Stack
import qualified Numeric.Natural
import qualified Prelude
import qualified System.Exit
import qualified Text.Read

yieldThread :: MonadIO m => m ()
yieldThread = UnliftIO.Concurrent.yield
{-# INLINE yieldThread #-}
