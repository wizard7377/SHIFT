module Main where

import Criterion (bgroup)
import Criterion.Main (defaultMain)
import Test.Tasty (testGroup)
import Tests

main :: IO ()
main = defaultMain [searchBench]
