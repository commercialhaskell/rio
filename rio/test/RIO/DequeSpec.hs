{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module RIO.DequeSpec (spec) where

import RIO
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Data.Vector.Generic.Mutable as V

data DequeAction
    = PushFront Int
    | PushBack Int
    | PopFront
    | PopBack
    deriving Show
instance Arbitrary DequeAction where
    arbitrary = oneof $ concat
        [ replicate 25 $ fmap PushFront arbitrary
        , replicate 25 $ fmap PushBack arbitrary
        , [return PopFront, return PopBack]
        ]

manyPushes :: [DequeAction]
manyPushes = concat
    [ replicate 50 $ PushBack 0
    , replicate 50 PopFront
    , replicate 50 $ PushFront 0
    , replicate 50 PopBack
    ]

specialCase :: [DequeAction]
specialCase =
    [PushBack 9, PushBack 5,PushBack 11,PushBack 2,PushBack 13,PushBack 10,PushBack 4,PushBack 13,PushBack 7,PushBack 8,PushBack 6,PushBack 4,PushBack 7,PushBack 9,PushBack 10,PushBack 3,PushBack 2,PushBack 12,PushBack 12 ,PushBack 6,PushBack 3,PushBack 5,PushBack 14,PushBack 14,PushBack 11,PushBack 8,PopFront,PopFront,PopFront,PushBack 11,PushBack 3,PopFront,PopFront,PushBack 13,PushBack 12,PopFront,PushBack 10,PushBack 7,PopFront,PopFront,PushBack 13,PushBack 9,PopFront,PushBack 7,PushBack 2,PopFront,PopFront,PushBack 6,PushBack 4,PopFront,PopFront,PopFront,PushBack 9,PushBack 3,PopFront,PushBack 10,PushBack 6,PopFront,PopFront,PopFront,PushBack 12,PushBack 5,PopFront,PushBack 12,PushBack 5,PopFront,PushBack 6,PushBack 4,PopFront,PopFront,PopFront,PushBack 14,PushBack 10,PopFront,PushBack 14,PushBack 10,PopFront,PushBack 11,PushBack 8,PopFront,PushBack 8,PushBack 2,PopFront,PopFront,PopFront,PushBack 13,PushBack 7,PopFront,PushBack 12,PushBack 5,PopFront,PushBack 10,PushBack 8, PopFront,PushBack 7,PushBack 2,PopFront,PopFront,PushBack 9,PushBack 4,PopFront,PopFront,PopFront,PopFront,PopFront,PopFront,PopFront,PopFront,PushBack 4,PushBack 9,PushBack 3,PushBack 10,PushBack 6,PushBack 4,PushBack 13,PushBack 7,PushBack 9,PushBack 3,PopFront]

spec :: Spec
spec = do
  let runActions
        :: V.MVector v Int
        => (Deque v (PrimState IO) Int -> Deque v (PrimState IO) Int)
        -> [DequeAction]
        -> IO ()
      runActions forceType actions = do
        base <- newIORef [] :: IO (IORef [Int])
        tested <- fmap forceType newDeque
        for_ (PopFront : PopBack : actions) $ \action -> do
          case action of
            PushFront i -> do
              pushFrontRef base i
              pushFrontDeque tested i
              same base tested
            PushBack i -> do
              pushBackRef base i
              pushBackDeque tested i
              same base tested
            PopFront -> do
              expected <- popFrontRef base
              actual <- popFrontDeque tested
              actual `shouldBe` expected
              same base tested
            PopBack -> do
              expected <- popBackRef base
              actual <- popBackDeque tested
              actual `shouldBe` expected
              same base tested
        let drain = do
              expected <- popBackRef base
              actual <- popBackDeque tested
              actual `shouldBe` expected
              case actual of
                Just _ -> drain
                Nothing -> return $! ()
        drain
      test name forceType = describe name $ do
        prop "arbitrary actions" $ runActions forceType
        it "many pushes" $ runActions forceType manyPushes
        it "special case" $ runActions forceType specialCase

  test "UDeque" asUDeque
  test "SDeque" asSDeque
  test "BDeque" asBDeque

pushFrontRef :: IORef [Int] -> Int -> IO ()
pushFrontRef ref i = modifyIORef ref (i:)

pushBackRef :: IORef [Int] -> Int -> IO ()
pushBackRef ref i = modifyIORef ref (++ [i])

popFrontRef :: IORef [Int] -> IO (Maybe Int)
popFrontRef ref = do
  is <- readIORef ref
  case is of
    i:is' -> do
      writeIORef ref is'
      pure $ Just i
    [] -> pure Nothing

popBackRef :: IORef [Int] -> IO (Maybe Int)
popBackRef ref = do
  is <- readIORef ref
  case reverse is of
    i:is' -> do
      writeIORef ref $ reverse is'
      pure $ Just i
    [] -> pure Nothing

same :: V.MVector v Int => IORef [Int] -> Deque v (PrimState IO) Int -> IO ()
same ref deque = do
  fromRef <- readIORef ref
  fromRight <- foldrDeque (\i rest -> pure $ i : rest) [] deque
  fromRight `shouldBe` fromRef
  fromLeft <- foldlDeque (\rest i -> pure $ i : rest) [] deque
  fromLeft `shouldBe` reverse fromRef
  dequeToList deque `shouldReturn` fromRef
