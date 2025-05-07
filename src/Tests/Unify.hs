module Tests.Unify where

import Rift
import Test.Tasty
import Test.Tasty.HUnit

unifyTests :: TestTree
unifyTests =
  testGroup
    "Unify tests"
    [ testCase "Basic unification 0" $ assertBool "0-0" $ (not . null) (unifyTest uA0 uB0)
    , testCase "Basic unification 1" $ assertBool "0-1" $ (not . null) (unifyTest uA1 uB1)
    ]
uA0 = genTest ["A"] "A"
uB0 = genTest [] "(x y [z] {f} _)"
uA1 = genTest ["x", "y"] "x + y = 3"
uB1 = genTest ["y", "z"] "2 + z = y"
