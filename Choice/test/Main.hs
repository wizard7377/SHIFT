module Main where

import Test.Common
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Choice Tests" [simpleTests, largeTests]
