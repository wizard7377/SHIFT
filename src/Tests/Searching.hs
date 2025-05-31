{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Searching (searchTests, searchBench, paeno, fundementalT, reallySimple) where

import Criterion
import Rift
import Sift
import Sift.Dev.Util (benchSolve, benchSolve', requireSolve, requireSolve')
import Test.Tasty
import Test.Tasty.HUnit

paeno :: [TestTerm]
paeno = selectAt (readSys "test/Simple.tift") [0 .. 3]
reallySimple = selectAt (readSys "test/Simple2.tift") [0]
fundementalT :: [TestTerm]
fundementalT = selectAt (readSys "test/Fundemental.tift") [0]
t0 :: TestTerm
t0 = tRead "(= 0 0)"
t1 :: TestTerm
t1 = tRead "(= (S 0) 1)"
t2 :: TestTerm
t2 = tRead "(= (S (S 0)) 2)"
t3 :: TestTerm
t3 = tRead "(Nat 0)"
t4 :: TestTerm
t4 = tRead "(Nat (S 0))"
t5 :: TestTerm
t5 = tRead "(Nat 1)"
searchTests :: TestTree
searchTests =
  testGroup
    "Search tests"
    [ testCase "Paeno 0" $ () <$ requireSolve' genSearch paeno t0
    , testCase "Paeno 1" $ () <$ requireSolve' genSearch paeno t1
    , testCase "Paeno 2" $ () <$ requireSolve' genSearch paeno t2
    , testCase "Paeno 3" $ () <$ requireSolve' genSearch paeno t3
    , testCase "Paeno 4" $ () <$ requireSolve' genSearch paeno t4
    , testCase "Paeno 5" $ () <$ requireSolve' genSearch paeno t5
    ]
searchBench =
  bgroup
    "Search tests"
    [ bench "Paeno 0" $ benchSolve' genSearch paeno t0
    , bench "Paeno 1" $ benchSolve' genSearch paeno t1
    , bench "Paeno 2" $ benchSolve' genSearch paeno t2
    , bench "Paeno 3" $ benchSolve' genSearch paeno t3
    , bench "Paeno 4" $ benchSolve' genSearch paeno t4
    , bench "Paeno 5" $ benchSolve' genSearch paeno t5
    ]
uA0 = genTest ["A"] "A"
uB0 = genTest [] "(x y [z] {f} _)"
uA1 = genTest ["x", "y"] "x + y = 3"
uB1 = genTest ["y", "z"] "2 + z = y"
