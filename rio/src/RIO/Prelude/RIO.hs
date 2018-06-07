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
  -- * SomeRef for Writer/State interfaces
  , SomeRef
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

instance PrimMonad (RIO env) where
    type PrimState (RIO env) = PrimState IO
    primitive = RIO . ReaderT . const . primitive

data SomeRef a
  = SomeRef !(IO a) !(a -> IO ())

readSomeRef :: SomeRef a -> IO a
readSomeRef (SomeRef x _) = x

writeSomeRef :: SomeRef a -> a -> IO ()
writeSomeRef (SomeRef _ x) = x

modifySomeRef :: SomeRef a -> (a -> a) -> IO ()
modifySomeRef (SomeRef read write) f =
  (f <$> read) >>= write

ioRefToSomeRef :: IORef a -> SomeRef a
ioRefToSomeRef ref = do
  SomeRef (readIORef ref)
          (\val -> modifyIORef' ref (\_ -> val))

uRefToSomeRef :: Unbox a => URef RealWorld a -> SomeRef a
uRefToSomeRef ref = do
  SomeRef (readURef ref) (writeURef ref)

class HasStateRef s env | env -> s where
  stateRefL :: Lens' env (SomeRef s)

instance HasStateRef a (SomeRef a) where
  stateRefL = lens id (\_ x -> x)

class HasWriteRef w env | env -> w where
  writeRefL :: Lens' env (SomeRef w)

instance HasWriteRef a (SomeRef a) where
  writeRefL = lens id (\_ x -> x)

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

newSomeRef :: MonadIO m => a -> m (SomeRef a)
newSomeRef a = do
  ioRefToSomeRef <$> newIORef a

newUnboxedSomeRef :: (MonadIO m, Unbox a) => a -> m (SomeRef a)
newUnboxedSomeRef a =
  uRefToSomeRef <$> (liftIO $ newURef a)
