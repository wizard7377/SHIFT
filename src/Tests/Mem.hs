{-# LANGUAGE DataKinds #-}

module Tests.Mem where

import Data.Text qualified as T
import Extra
import Extra.TestHelp (makeTestList)
import Rift qualified
import Rift.Core.Interface (simplifyF)
import Test.HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

makeMemName ::
  -- | VUP
  String ->
  -- | TUPFROM
  String ->
  -- | TUPTO
  String ->
  -- | VDown
  String ->
  -- | TDOWN
  String ->
  [(String, String)] ->
  String
makeMemName vUp tUpFrom tUpTo vDown tDown tr =
  let
    vUp' = makeTermList vUp
    tUpFrom' = Rift.tRead tUpFrom
    tUpTo' = Rift.tRead tUpTo
    vDown' = makeTermList vDown
    tDown' = Rift.tRead tDown
    tr' = (uncurry makeFTermTest) <$> tr
   in
    show $ "mem " ++ show vUp' ++ " " ++ show tUpFrom' ++ " " ++ show tUpTo' ++ " " ++ show vDown' ++ " " ++ show tDown' ++ " " ++ show tr'
makeMemMsg vUp tUpFrom tUpTo vDown tDown tr =
  let
    vUp' = makeTermList vUp
    tUpFrom' = Rift.tRead tUpFrom
    tUpTo' = Rift.tRead tUpTo
    vDown' = makeTermList vDown
    tDown' = Rift.tRead tDown
    tr' = (uncurry makeFTermTest) <$> tr
   in
    show $ show $ "Couldn't solve for" ++ show tr' ++ " from מ: ∀" ++ show vUp' ++ " . " ++ show tUpFrom' ++ " |- " ++ show tUpTo' ++ " of : ∀ " ++ show vDown' ++ " . " ++ show tDown' ++ " "
makeTermList :: String -> [Rift.TestTerm]
makeTermList = makeTestList (Rift.tRead . T.unpack) ";"
makeFTermTest :: String -> String -> Rift.FTerm' Rift.TestTerm
makeFTermTest t s =
  let
    t' = Rift.tRead t
    s' = makeTermList s
   in
    Rift.FTerm' t' s'

makeMemTest ::
  -- | VUP
  String ->
  -- | TUPFROM
  String ->
  -- | TUPTO
  String ->
  -- | VDown
  String ->
  -- | TDOWN
  String ->
  Choice (Rift.FTerm' Rift.TestTerm)
makeMemTest vUp tUpFrom tUpTo vDown tDown =
  let
    vUp' = makeTermList vUp
    tUpFrom' = Rift.tRead tUpFrom
    tUpTo' = Rift.tRead tUpTo
    vDown' = makeTermList vDown
    tDown' = Rift.tRead tDown
   in
    simplifyF <$> Rift.memReduce 0 vUp' tUpFrom' tUpTo' (makeFTermTest tDown vDown)
memTesting ::
  -- | VUP
  String ->
  -- | TUPFROM
  String ->
  -- | TUPTO
  String ->
  -- | VDown
  String ->
  -- | TDOWN
  String ->
  [(String, String)] ->
  -- | Name
  String ->
  TestTree
memTesting vUp tUpFrom tUpTo vDown tDown tr name =
  let
    res = makeMemTest vUp tUpFrom tUpTo vDown tDown
    tr' = (uncurry makeFTermTest) <$> tr
   in
    testCase name $ assertBool (makeMemMsg vUp tUpFrom tUpTo vDown tDown tr) (csubset' res (mkChoice tr'))

{-
memTest :: [String] -> String -> String -> [String] -> String -> [([String], String)] -> Assertion
memTest vUp tUpFrom tUpTo vDown tDown hypos =
  let
    res = Rift.mem' (Rift.tRead tUpFrom) (Rift.tRead tUpTo) (Rift.tRead <$> vUp) (Rift.tRead tDown) (Rift.tRead <$> vDown)
    hypos' = (\(x, y) -> (Rift.FTerm' (Rift.tRead y)) (Rift.tRead <$> x)) <$> hypos
   in
    res @?= (mkChoice hypos')
-}
basicMem :: TestTree
basicMem =
  testGroup
    "Basic mem"
    [ memTesting "" "x" "y" "" "x" [("y", "")] "M1"
    , memTesting "x" "x" "y" "" "z" [("y", "")] "M2"
    , memTesting "x" "x" "x" "" "z" [("z", "")] "M3"
    , memTesting "x;y" "(= x y)" "(= y x)" "" "(= 1 (S 0))" [("(= (S 0) 1)", "")] "M4"
    -- TODO these need better naming system
    -- , memTesting "x;y" "(= x y)" "(= y x)" "a" "(= a 1)" [("(= 1 x)", "x")] "M5"
    -- , memTesting "x;y" "(= x y)" "(= y x)" "a" "(= a (S b))" [("(= (S b) x)", "x;b")] "M6"
    ]
