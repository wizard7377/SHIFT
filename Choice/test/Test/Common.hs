module Test.Common where

import Data.Choice
import Data.Foldable (Foldable (..))
import Test.Tasty
import Test.Tasty.HUnit

type TestChoice = ChoiceT IO
assertChoice :: (Show a) => TestChoice a -> [Int] -> IO Int
assertChoice c s = do
  res <- runChoiceT c
  if ((length res) `elem` s)
    then pure (length res)
    else error $ "Expected length in " ++ show s ++ ", got result " ++ show (res) ++ " with length " ++ show (length res)

selectChoiceTest :: (Show c) => (a -> b -> c) -> [a] -> [b] -> [Int] -> IO ()
selectChoiceTest f as bs s =
  ( assertChoice
      ( do
          a' <- csplit $ pure as
          b' <- csplit $ pure bs
          let res = f a' b'
          pure res
      )
      s
  )
    >> pure ()
selectChoiceTestPlus :: (Show c, Foldable f) => (a -> b -> f c) -> [a] -> [b] -> [Int] -> IO ()
selectChoiceTestPlus f as bs s =
  ( assertChoice
      ( do
          a' <- csplit $ pure as
          b' <- csplit $ pure bs
          let res = toList $ f a' b'
          res' <- csplit $ pure res
          pure res'
      )
      s
  )
    >> pure ()

testOp :: (Enum a) => (Num a) => a -> a -> [a]
testOp !a !b = [(a - b) .. (a + b)]

testOpB :: (Enum a, Num a) => (Num a) => (MonadChoice m) => m [a] -> m _
testOpB !c = cfilter (flip all $ (< 20))
simpleTests :: TestTree
simpleTests =
  testGroup
    "Simple Tests"
    [ testCase "Select Choice Test 1" $
        (selectChoiceTest (+) [1, 2, 3] [4, 5] [6])
    , testCase
        "Select Choice Test 2"
        $ (selectChoiceTest (*) [1, 2, 3] [4, 5] [6])
    , testCase
        "Select Choice Test 3"
        $ (selectChoiceTest (>) [1, 2, 3] [4, 5] [6])
    ]

largeTests :: TestTree
largeTests =
  testGroup
    "Large Tests"
    [ testCase "Select Choice Test Plus 1" $
        (selectChoiceTestPlus testOp [0 .. 20] [0 .. 20] [9261])
    ]
