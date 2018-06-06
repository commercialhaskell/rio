{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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

import GHC.Exts (Constraint)

import RIO.Prelude.URef
import RIO.Prelude.Lens
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
  = SomeRef { readSomeRef :: !(IO a)
            , writeSomeRef :: !(a -> IO ())
            }

modifySomeRef :: SomeRef a -> (a -> a) -> IO ()
modifySomeRef (SomeRef read write) f =
  (f <$> read) >>= write

{-

NOTE:

I believe there is still some merit to this idea, leaving for now
to assess and discuss

class ToSomeRef (ref :: * -> *) where
  type RefConstraints ref a ::  Constraint
  toSomeRef :: RefConstraints ref a => ref a -> SomeRef a

instance ToSomeRef IORef where
  type RefConstraints IORef a = (() :: Constraint)
  toSomeRef = ioRefToSomeRef

instance (PrimState IO ~ base) => ToSomeRef (URef base) where
  type RefConstraints (URef base) a = Unbox a
  toSomeRef = uRefToSomeRef

runRIORef
  :: (ToSomeRef ref, MonadIO m, RefConstraints ref a)
  => ref a -> RIO (SomeRef a) b -> m b
runRIORef env action = runRIO (toSomeRef env) action

-- ref <- newURef 0
-- runRIORef ref $ do
--   modify (+1)
-- readURef ref ==> 1

-}

ioRefToSomeRef ref = do
  SomeRef (readIORef ref)
          (\val -> atomicModifyIORef' ref (\old -> (val, ())))


uRefToSomeRef ref = do
  SomeRef (readURef ref) (writeURef ref)


instance MonadState s (RIO (SomeRef s)) where
  get = ask >>= liftIO . readSomeRef
  put st = do
    ref <- ask
    liftIO $ writeSomeRef ref st

instance Monoid w => MonadWriter w (RIO (SomeRef w)) where
  tell value = do
    ref <- ask
    liftIO $ modifySomeRef ref (`mappend` value)

  listen action = do
    (,) <$> action <*> (ask >>= liftIO . readSomeRef)

  pass action = do
    (a, transF) <- action
    ref <- ask
    liftIO $ modifySomeRef ref transF
    return a

newSomeRef :: MonadIO m => a -> m (SomeRef a)
newSomeRef a = do
  ioRefToSomeRef <$> newIORef a

newUnboxedSomeRef :: (MonadIO m, Unbox a) => a -> m (SomeRef a)
newUnboxedSomeRef a =
  uRefToSomeRef <$> (liftIO $ newURef a)

-- acc <- newIORef mempty
-- runRIO (ioRefToSomeRef acc) $ do
-- ref <- newUnboxedSomeRef mempty
-- runRIO ref $ do
--   tell "Hello World"
--   tell "Hola Mundo"
-- readSomeRef ref ==> "Hello WorldHola Mundo"
