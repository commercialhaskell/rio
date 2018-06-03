{-# LANGUAGE BangPatterns #-}
module RIO.Prelude.Extra
  ( mapLeft
  , fromFirst
  , mapMaybeA
  , mapMaybeM
  , forMaybeA
  , forMaybeM
  , foldMapM
  , nubOrd
  , whenM
  , unlessM
  , asIO
  ) where

import qualified Data.Set as Set
import Data.Monoid (First (..))
import RIO.Prelude.Reexports

-- | Apply a function to a 'Left' constructor
mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f (Left a1) = Left (f a1)
mapLeft _ (Right b) = Right b

-- | Get a 'First' value with a default fallback
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

-- | Extend 'foldMap' to allow side effects.
--
-- @since 0.1.3.0
foldMapM
  :: (Applicative f, Monoid m, Foldable t)
  => (a -> f m)
  -> t a
  -> f m
foldMapM f = runFMHelper . foldMap (FMHelper . f)

newtype FMHelper f a = FMHelper { runFMHelper :: f a }
instance (Applicative f, Semigroup a) => Semigroup (FMHelper f a) where
  FMHelper x <> FMHelper y = FMHelper (liftA2 (<>) x y)
instance (Applicative f, Monoid a) => Monoid (FMHelper f a) where
  mempty = FMHelper (pure mempty)
  mappend (FMHelper x) (FMHelper y) = FMHelper (liftA2 mappend x y)

-- | Strip out duplicates
nubOrd :: Ord a => [a] -> [a]
nubOrd =
  loop mempty
  where
    loop _ [] = []
    loop !s (a:as)
      | a `Set.member` s = loop s as
      | otherwise = a : loop (Set.insert a s) as

-- | Run the second value if the first value returns 'True'
whenM :: Monad m => m Bool -> m () -> m ()
whenM boolM action = do
  x <- boolM
  if x then action else return ()

-- | Run the second value if the first value returns 'False'
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM boolM action = do
  x <- boolM
  if x then return () else action

-- | Helper function to force an action to run in 'IO'. Especially
-- useful for overly general contexts, like hspec tests.
--
-- @since 0.1.3.0
asIO :: IO a -> IO a
asIO = id
