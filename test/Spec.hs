module Main where

import Test.Tasty (defaultMain, testGroup)
import Tests

main :: IO ()
main = defaultMain $ testGroup "SHIFT tests" [searchTests]
