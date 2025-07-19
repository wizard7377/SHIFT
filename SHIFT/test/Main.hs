module Main where

import Test.Tasty (DependencyType (..), defaultMain, sequentialTestGroup, testGroup)
import Test.Tasty.HUnit
import Tests

main :: IO ()
main = defaultMain $ sequentialTestGroup "SHIFT tests" AllFinish [unifyTests, convertCases]
