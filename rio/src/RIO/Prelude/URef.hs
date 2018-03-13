module RIO.Prelude.URef
  ( -- * Unboxed references
    Unbox
  , URef
  , IOURef
  , newURef
  , readURef
  , writeURef
  , modifyURef
  ) where

import RIO.Prelude.Reexports
import qualified Data.Vector.Unboxed.Mutable as MUVector

-- | An unboxed reference. This works like an 'IORef', but the data is
-- stored in a bytearray instead of a heap object, avoiding
-- significant allocation overhead in some cases. For a concrete
-- example, see this Stack Overflow question:
-- <https://stackoverflow.com/questions/27261813/why-is-my-little-stref-int-require-allocating-gigabytes>.
--
-- The first parameter is the state token type, the same as would be
-- used for the 'ST' monad. If you're using an 'IO'-based monad, you
-- can use the convenience 'IOURef' type synonym instead.
--
-- @since 0.0.2.0
newtype URef s a = URef (MUVector.MVector s a)

-- | Helpful type synonym for using a 'URef' from an 'IO'-based stack.
--
-- @since 0.0.2.0
type IOURef = URef (PrimState IO)

-- | Create a new 'URef'
--
-- @since 0.0.2.0
newURef :: (PrimMonad m, Unbox a) => a -> m (URef (PrimState m) a)
newURef a = fmap URef (MUVector.replicate 1 a)

-- | Read the value in a 'URef'
--
-- @since 0.0.2.0
readURef :: (PrimMonad m, Unbox a) => URef (PrimState m) a -> m a
readURef (URef v) = MUVector.unsafeRead v 0

-- | Write a value into a 'URef'. Note that this action is strict, and
-- will force evalution of the value.
--
-- @since 0.0.2.0
writeURef :: (PrimMonad m, Unbox a) => URef (PrimState m) a -> a -> m ()
writeURef (URef v) = MUVector.unsafeWrite v 0

-- | Modify a value in a 'URef'. Note that this action is strict, and
-- will force evaluation of the result value.
--
-- @since 0.0.2.0
modifyURef :: (PrimMonad m, Unbox a) => URef (PrimState m) a -> (a -> a) -> m ()
modifyURef u f = readURef u >>= writeURef u . f
