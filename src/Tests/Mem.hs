{-# LANGUAGE DataKinds #-}

module Tests.Mem where

import Rift qualified
import Test.HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

memTest :: [String] -> String -> String -> [String] -> String -> [([String], String)] -> Assertion
memTest vUp tUpFrom tUpTo vDown tDown hypos =
  let
    res = Rift.mem' (Rift.tRead tUpFrom) (Rift.tRead tUpTo) (Rift.tRead <$> vUp) (Rift.tRead tDown) (Rift.tRead <$> vDown)
    hypos' = (\(x, y) -> (Rift.FTerm (Rift.tRead y)) (Rift.tRead <$> x)) <$> hypos
   in
    res @?= hypos'

basicMem :: TestTree
basicMem =
  testGroup "Basic mem" $
    [ testCase "Unlike assertion" $ memTest [] "x" "0" [] "y" []
    , testCase "Unlike assertion" $ memTest ["z"] "x" "0" [] "y" []
    , testCase "Unlike assertion" $ memTest ["z"] "x" "0" ["a"] "y" []
    , testCase "Simple match" $ memTest [] "x" "0" [] "x" []
    ]
