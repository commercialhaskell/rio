module RIO.Prelude
  (
    module RIO.Prelude.Types
    -- * @Bool@
    -- | Re-exported from "Data.Bool":
  , (Data.Bool.||)
  , (Data.Bool.&&)
  , Data.Bool.not
  , Data.Bool.otherwise
  , Data.Bool.bool

    -- * @Maybe@
    -- | Re-exported from "Data.Maybe":
  , Data.Maybe.maybe
  , Data.Maybe.fromMaybe
  , RIO.Prelude.Extra.fromFirst
  , Data.Maybe.isJust
  , Data.Maybe.isNothing
  , Data.Maybe.listToMaybe
  , Data.Maybe.maybeToList
  , Data.Maybe.catMaybes
  , Data.Maybe.mapMaybe
  , RIO.Prelude.Extra.mapMaybeA
  , RIO.Prelude.Extra.mapMaybeM
  , RIO.Prelude.Extra.forMaybeA
  , RIO.Prelude.Extra.forMaybeM

    -- * @Either@
    -- | Re-exported from "Data.Either":
  , Data.Either.either
  , Data.Either.isLeft
  , Data.Either.isRight
  , RIO.Prelude.Extra.mapLeft
  , Data.Either.lefts
  , Data.Either.partitionEithers
  , Data.Either.rights

    -- * Tuples
    -- | Re-exported from "Data.Tuple":
  , Data.Tuple.fst
  , Data.Tuple.snd
  , Data.Tuple.curry
  , Data.Tuple.uncurry
  -- , Data.Tuple.swap -- TODO: export?

    -- * @Eq@
    -- | Re-exported from "Data.Eq":
  , (Data.Eq.==)
  , (Data.Eq./=)

    -- * @Ord@
    -- | Re-exported from "Data.Ord":
  , (Data.Ord.<)
  , (Data.Ord.<=)
  , (Data.Ord.>)
  , (Data.Ord.>=)
  , Data.Ord.max
  , Data.Ord.min
  , Data.Ord.compare
  , Data.Ord.comparing
  , Data.Ord.Down(..)

    -- * @Enum@
    -- | Re-exported from "Prelude":
  , Prelude.fromEnum

    -- * @Bounded@
    -- | Re-exported from "Prelude":
  , Prelude.minBound
  , Prelude.maxBound

    -- * @Num@
    -- | Re-exported from "Prelude":
  , (Prelude.+)
  , (Prelude.-)
  , (*) -- HSE can't parse qualified export, which results in hlint error.
  , (Prelude.^)
  , Prelude.negate
  , Prelude.abs
  , Prelude.signum
  , Prelude.fromInteger
  , Prelude.subtract

    -- * @Real@
    -- | Re-exported from "Prelude":
  , Prelude.toRational

    -- * @Integral@
    -- | Re-exported from "Prelude":
  , Prelude.quot
  , Prelude.rem
  , Prelude.div
  , Prelude.mod
  , Prelude.quotRem
  , Prelude.divMod
  , Prelude.toInteger
  , Prelude.even
  , Prelude.odd
  , Prelude.gcd
  , Prelude.lcm
  , Prelude.fromIntegral

    -- * @Fractional@
    -- | Re-exported from "Prelude":
  , (Prelude./)
  , (Prelude.^^)
  , Prelude.recip
  , Prelude.fromRational
  , Prelude.realToFrac

    -- * @Floating@
    -- | Re-exported from "Prelude":
  , Prelude.pi
  , Prelude.exp
  , Prelude.log
  , Prelude.sqrt
  , (Prelude.**)
  , Prelude.logBase
  , Prelude.sin
  , Prelude.cos
  , Prelude.tan
  , Prelude.asin
  , Prelude.acos
  , Prelude.atan
  , Prelude.sinh
  , Prelude.cosh
  , Prelude.tanh
  , Prelude.asinh
  , Prelude.acosh
  , Prelude.atanh

    -- * @RealFrac@
    -- | Re-exported from "Prelude":
  , Prelude.properFraction
  , Prelude.truncate
  , Prelude.round
  , Prelude.ceiling
  , Prelude.floor

    -- * @RealFloat@
    -- | Re-exported from "Prelude":
  , Prelude.floatRadix
  , Prelude.floatDigits
  , Prelude.floatRange
  , Prelude.decodeFloat
  , Prelude.encodeFloat
  , Prelude.exponent
  , Prelude.significand
  , Prelude.scaleFloat
  , Prelude.isNaN
  , Prelude.isInfinite
  , Prelude.isDenormalized
  , Prelude.isNegativeZero
  , Prelude.isIEEE
  , Prelude.atan2

    -- * @Word@
    -- | Re-exported from "Data.Word":
  , Data.Word.byteSwap16
  , Data.Word.byteSwap32
  , Data.Word.byteSwap64

    -- * @Semigroup@
    -- | Re-exported from "Data.Semigroup":
  , (Data.Semigroup.<>)
  , RIO.Prelude.Renames.sappend

    -- * @Monoid@
    -- | Re-exported from "Data.Monoid":
  , Data.Monoid.mempty
  , Data.Monoid.mappend
  , Data.Monoid.mconcat

    -- * @Functor@
    -- | Re-exported from "Data.Functor":
  , Data.Functor.fmap
  , (Data.Functor.<$>)
  , (Data.Functor.<$)
  , (Data.Functor.$>)
  , Data.Functor.void
  , (RIO.Prelude.Extra.<&>)

    -- * @Applicative@
    -- | Re-exported from "Control.Applicative":
  , Control.Applicative.pure
  , (Control.Applicative.<*>)
  , (Control.Applicative.<*)
  , (Control.Applicative.*>)
  , Control.Applicative.liftA
  , Control.Applicative.liftA2
  , Control.Applicative.liftA3
  , Control.Monad.forever
  , Data.Foldable.traverse_
  , Data.Foldable.for_
  , Data.Foldable.sequenceA_
  , Control.Monad.filterM
  , Control.Monad.replicateM_
  , Control.Monad.zipWithM
  , Control.Monad.zipWithM_

    -- * @Monad@
    -- | Re-exported from "Control.Monad":
  , Control.Monad.return
  , Control.Monad.join
  , Control.Monad.fail
  , (Control.Monad.>>=)
  , (Control.Monad.>>)
  , (Control.Monad.=<<)
  , (Control.Monad.>=>)
  , (Control.Monad.<=<)
  , (Control.Monad.<$!>)
  , Control.Monad.liftM
  , Control.Monad.liftM2
  , RIO.Prelude.Extra.whenM
  , RIO.Prelude.Extra.unlessM
  , Data.Foldable.mapM_
  , Data.Foldable.forM_
  , Data.Foldable.sequence_

  , Control.Monad.foldM
  , Control.Monad.foldM_
  -- TODO: Export these as well perhaps?
  -- , Data.Foldable.foldlM
  -- , Data.Foldable.foldrM

    -- * @Foldable@
    -- | Re-exported from "Data.Foldable":
  , Data.Foldable.foldr
  , Data.Foldable.foldl'
  , Data.Foldable.fold
  , Data.Foldable.foldMap
  , RIO.Prelude.Extra.foldMapM
  , Data.Foldable.elem
  , Data.Foldable.notElem
  , Data.Foldable.null
  , Data.Foldable.length
  , Data.Foldable.sum
  , Data.Foldable.product
  , Data.Foldable.all
  , Data.Foldable.any
  , Data.Foldable.and
  , Data.Foldable.or
  , Data.Foldable.toList
  , Data.Foldable.concat
  , Data.Foldable.concatMap

    -- * @Traversable@
    -- | Re-exported from "Data.Traversable":
  , Data.Traversable.traverse
  , Data.Traversable.for
  , Data.Traversable.sequenceA
  , Data.Traversable.mapM
  , Data.Traversable.forM
  , Data.Traversable.sequence

    -- * @Alternative@
    -- | Re-exported from "Control.Applicative":
  , (Control.Applicative.<|>)
  , aempty -- ^ @since 0.1.10.1
  , Control.Applicative.some
  , Control.Applicative.many
  , Control.Applicative.optional
  , Data.Foldable.asum
  , Control.Monad.guard
  , Control.Monad.when
  , Control.Monad.unless

    -- * @MonadPlus@
    -- | Re-exported from "Control.Monad":
  , Control.Monad.mzero
  , Control.Monad.mplus
  , Control.Monad.msum
  , Control.Monad.mfilter

    -- * @Arrow@
    -- | Re-exported from "Control.Arrow" and "Control.Category":
  , Control.Arrow.first
  , Control.Arrow.second
  , (Control.Arrow.&&&)
  , (Control.Arrow.***)
  , (Control.Category.>>>)

    -- * @Function@
    -- | Re-exported from "Data.Function":
  , Data.Function.id
  , Data.Function.const
  , (Data.Function..)
  , (Data.Function.$)
  , (Data.Function.&)
  , Data.Function.flip
  , Data.Function.fix
  , Data.Function.on

    -- * Miscellaneous functions
  , (Prelude.$!)
  , Prelude.seq
  , Prelude.error
  , Prelude.undefined
  , Prelude.asTypeOf
  , RIO.Prelude.Extra.asIO

    -- * List
    -- | Re-exported from "Data.List":
  , (Data.List.++)
  , Data.List.break
  , Data.List.drop
  , Data.List.dropWhile
  , Data.List.filter
  , Data.List.lookup
  , Data.List.map
  , Data.List.replicate
  , Data.List.reverse
  , Data.List.span
  , Data.List.take
  , Data.List.takeWhile
  , Data.List.zip
  , Data.List.zipWith
  , RIO.Prelude.Extra.nubOrd


    -- * @String@
    -- | Re-exported from "Data.String":
  , Data.String.fromString
  , Data.String.lines
  , Data.String.unlines
  , Data.String.unwords
  , Data.String.words

    -- ** @Show@
    -- | Re-exported from "Text.Show":
  , Text.Show.show
    -- ** @Read@
    -- | Re-exported from "Text.Read":
  , Text.Read.readMaybe

    -- * @NFData@

    -- | Re-exported from "Control.DeepSeq":
  , (Control.DeepSeq.$!!)
  , Control.DeepSeq.rnf
  , Control.DeepSeq.deepseq
  , Control.DeepSeq.force

    -- * @Void@
    -- | Re-exported from "Data.Void":
  , Data.Void.absurd

    -- * @Reader@
    -- | Re-exported from "Control.Monad.Reader":
  , Control.Monad.Reader.lift
  , Control.Monad.Reader.ask
  , Control.Monad.Reader.asks
  , Control.Monad.Reader.local
  , Control.Monad.Reader.runReader
  , Control.Monad.Reader.runReaderT

    -- * @ByteString@
    -- | Helper synonyms for converting bewteen lazy and strict @ByteString@s
  , RIO.Prelude.Renames.toStrictBytes
  , RIO.Prelude.Renames.fromStrictBytes

    -- * @ShortByteString@
    -- | Re-exported from "Data.ByteString.Short":
  , Data.ByteString.Short.toShort
  , Data.ByteString.Short.fromShort

    -- * @Text@
  , RIO.Prelude.Text.tshow
  , RIO.Prelude.Text.decodeUtf8Lenient
    -- | Re-exported from "Data.Text.Encoding":
  , Data.Text.Encoding.decodeUtf8'
  , Data.Text.Encoding.decodeUtf8With
  , Data.Text.Encoding.encodeUtf8
  , Data.Text.Encoding.encodeUtf8Builder
  , Data.Text.Encoding.Error.lenientDecode

    -- * @PrimMonad@
    -- | Re-exported from "Control.Monad.Primitive":
  , Control.Monad.Primitive.primitive
    -- | Re-exported from "Control.Monad.ST":
  , Control.Monad.ST.runST
  ) where

import qualified RIO.Prelude.Extra
import qualified RIO.Prelude.Renames
import qualified RIO.Prelude.Text
import qualified RIO.Prelude.Types

import Prelude ((*))
import qualified Prelude

import qualified Data.Bool

import qualified Data.Maybe

import qualified Data.Either

import qualified Data.Tuple

import qualified Data.Eq

import qualified Data.Ord

import qualified Data.Word

import qualified Data.Semigroup

import qualified Data.Monoid

import qualified Data.Functor

import qualified Control.Applicative

import qualified Control.Monad

import qualified Data.Foldable

import qualified Data.Traversable

import qualified Control.Arrow
import qualified Control.Category

import qualified Data.Function

import qualified Data.List

import qualified Data.String

import qualified Text.Show

import qualified Text.Read

import qualified Control.DeepSeq

import qualified Data.Void

import qualified Control.Monad.Reader

import qualified Data.ByteString.Short

import qualified Data.Text.Encoding (decodeUtf8', decodeUtf8With, encodeUtf8,
                                     encodeUtf8Builder)
import qualified Data.Text.Encoding.Error (lenientDecode)

import qualified Control.Monad.Primitive (primitive)
import qualified Control.Monad.ST

aempty :: Control.Applicative.Alternative f => f a
aempty = Control.Applicative.empty
