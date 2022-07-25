{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module RIO.Prelude.RIO
  ( RIO (..)
  , runRIO
  , liftRIO
  , mapRIO
  -- SomeRef for Writer/State interfaces
  , SomeRef
  , HasStateRef (..)
  , HasWriteRef (..)
  , newSomeRef
  , newUnboxedSomeRef
  , readSomeRef
  , writeSomeRef
  , modifySomeRef
  ) where

import GHC.Exts (RealWorld)

import RIO.Prelude.Lens
import RIO.Prelude.URef
import RIO.Prelude.Reexports
import Control.Monad.State (MonadState(..))
import Control.Monad.Writer (MonadWriter(..))

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

instance Semigroup a => Semigroup (RIO env a) where
  (<>) = liftA2 (<>)
instance Monoid a => Monoid (RIO env a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-- | Using the environment run in IO the action that requires that environment.
--
-- @since 0.0.1.0
runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO (f env)

-- | Abstract `RIO` to an arbitrary `MonadReader` instance, which can handle IO.
--
-- @since 0.0.1.0
liftRIO :: (MonadIO m, MonadReader env m) => RIO env a -> m a
liftRIO rio = do
  env <- ask
  runRIO env rio

-- | Lift one RIO env to another.
--
-- @since 0.1.13.0
mapRIO :: (outer -> inner) -> RIO inner a -> RIO outer a
mapRIO f m = do
  outer <- ask
  runRIO (f outer) m

instance MonadUnliftIO (RIO env) where
  withRunInIO inner = RIO $ withRunInIO $ \run -> inner (run . unRIO)
  {-# INLINE withRunInIO #-}

instance PrimMonad (RIO env) where
    type PrimState (RIO env) = PrimState IO
    primitive = RIO . ReaderT . const . primitive

-- | Abstraction over how to read from and write to a mutable reference
--
-- @since 0.1.4.0
data SomeRef a
  = SomeRef !(IO a) !(a -> IO ())

-- | Read from a SomeRef
--
-- @since 0.1.4.0
readSomeRef :: MonadIO m => SomeRef a -> m a
readSomeRef (SomeRef x _) = liftIO x

-- | Write to a SomeRef
--
-- @since 0.1.4.0
writeSomeRef :: MonadIO m => SomeRef a -> a -> m ()
writeSomeRef (SomeRef _ x) = liftIO . x

-- | Modify a SomeRef
-- This function is subject to change due to the lack of atomic operations
--
-- @since 0.1.4.0
modifySomeRef :: MonadIO m => SomeRef a -> (a -> a) -> m ()
modifySomeRef (SomeRef read' write) f =
  liftIO $ (f <$> read') >>= write

ioRefToSomeRef :: IORef a -> SomeRef a
ioRefToSomeRef ref =
  SomeRef (readIORef ref)
          (\val -> modifyIORef' ref (\_ -> val))

uRefToSomeRef :: Unbox a => URef RealWorld a -> SomeRef a
uRefToSomeRef ref =
  SomeRef (readURef ref) (writeURef ref)

-- | Environment values with stateful capabilities to SomeRef
--
-- @since 0.1.4.0
class HasStateRef s env | env -> s where
  stateRefL :: Lens' env (SomeRef s)

-- | Identity state reference where the SomeRef is the env
--
-- @since 0.1.4.0
instance HasStateRef a (SomeRef a) where
  stateRefL = id

-- | Environment values with writing capabilities to SomeRef
--
-- @since 0.1.4.0
class HasWriteRef w env | env -> w where
  writeRefL :: Lens' env (SomeRef w)

-- | Identity write reference where the SomeRef is the env
--
-- @since 0.1.4.0
instance HasWriteRef a (SomeRef a) where
  writeRefL = id

instance HasStateRef s env => MonadState s (RIO env) where
  get = do
    ref <- view stateRefL
    liftIO $ readSomeRef ref
  put st = do
    ref <- view stateRefL
    liftIO $ writeSomeRef ref st

instance (Monoid w, HasWriteRef w env) => MonadWriter w (RIO env) where
  tell value = do
    ref <- view writeRefL
    liftIO $ modifySomeRef ref (`mappend` value)

  listen action = do
    w1 <- view writeRefL >>= liftIO . readSomeRef
    a <- action
    w2 <- do
      refEnv <- view writeRefL
      v <- liftIO $ readSomeRef refEnv
      _ <- liftIO $ writeSomeRef refEnv w1
      return v
    return (a, w2)

  pass action = do
    (a, transF) <- action
    ref <- view writeRefL
    liftIO $ modifySomeRef ref transF
    return a

-- | create a new boxed SomeRef
--
-- @since 0.1.4.0
newSomeRef :: MonadIO m => a -> m (SomeRef a)
newSomeRef a = do
  ioRefToSomeRef <$> newIORef a

-- | create a new unboxed SomeRef
--
-- @since 0.1.4.0
newUnboxedSomeRef :: (MonadIO m, Unbox a) => a -> m (SomeRef a)
newUnboxedSomeRef a =
  uRefToSomeRef <$> (liftIO $ newURef a)
