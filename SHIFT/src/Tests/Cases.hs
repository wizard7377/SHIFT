module Tests.Cases where

import Data.Default
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Convert

convertCases :: TestTree
convertCases =
  testGroup
    "Simple converts"
    [ testGroup
        "Simple Convert Success"
        [ testCase "Null 1" $ convertTest def "x" "x"
        , testCase "Null 2" $ convertTest def "y" "y"
        , testCase "Null 3" $ convertTest def "(x x)" "(x x)"
        , testCase "Null 4" $ convertTest def "(x y)" "(x y)"
        ]
    , testGroup
        "Simple Convert Failures"
        [ testCase "Null 1" $ convertFailTest def "x" "y"
        , testCase "Null 2" $ convertFailTest def "x" "(x y)"
        , testCase "Null 3" $ convertFailTest def "(x y)" "(y x)"
        ]
    , testGroup
        "Getting more complex"
        [ testCase "Fix 1" $ convertTest def "{0} (x @0)" "(x {0}(x @0))"
        , testCase "Fix fail 1" $ convertFailTest def "{0} (x @0)" "(y {0}(x @0))"
        , testCase "Mem 1" $ convertTest def "((? x ⊥ x (x x)) y)" "(y y)"
        ]
    ]
