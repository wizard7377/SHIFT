{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Tests.Mem where

import Control.Lens qualified as Lens
import Data.Foldable (Foldable (toList))
import Data.Text qualified as T
import Debug.Trace (traceShowId)
import Extra
import Extra.Parsers
import Rift qualified
import Sift qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Tests.Common

memTestMsg ::
  (Plated Rift.TestTerm) =>
  [] (Rift.TestTerm) ->
  Rift.TestTerm ->
  Rift.TestTerm ->
  String
memTestMsg res input expected = show input ++ " does not מ-reduce to " ++ show expected ++ " (actually מ-reduces to: " ++ show (toList res) ++ " #" ++ show (length res) ++ " )"
memTestGen ::
  (Plated Rift.TestTerm) =>
  -- | Input
  String ->
  -- | Expected
  String ->
  Assertion
memTestGen = memTestGen' (Sift.reduceRec)

memTestGen' :: (Rift.Inner Rift.TestTerm ~ Rift.TestTerm, Rift.TermOf t ~ Rift.TestTerm) => (Rift.TestTerm -> Sift.OpM' t (Rift.TermOf t)) -> String -> String -> Assertion
memTestGen' = memTestGen'' $ \expect res -> any (\x -> not . null $ Sift.runOpMDef (Sift.convert x expect)) res
memTestGen'' ::
  (Plated Rift.TestTerm) =>
  (Rift.TestTerm -> [Rift.TestTerm] -> Bool) ->
  (Rift.TestTerm -> Sift.OpM Rift.TestTerm) ->
  -- | Input
  String ->
  -- | Expected
  String ->
  Assertion
memTestGen'' valid f input' expected' = do
  input <- termRead input'
  expected <- termRead expected'
  let res0 = f input
  let res1 = simplifyTest <$> res0
  let res2 = Sift.runOpM res1 def
  let isGood = valid expected res2
  assertBool (memTestMsg (res2) input expected) isGood

memTestRec :: (Plated Rift.TestTerm) => String -> String -> Assertion
memTestRec =
  let
    res = Sift.reduceRec
   in
    memTestGen' res
simplifyTest :: (Plated Rift.TestTerm) => Rift.TestTerm -> Rift.TestTerm
simplifyTest = Lens.transform (\case Rift.PrimFree t [] -> simplifyTest t; t -> t)
memTests :: (Plated Rift.TestTerm) => TestTree
memTests =
  testGroup
    "Mem tests"
    [ testCase "Simple mem reduction" $ memTestGen "((? x F x x) 1)" "1"
    , testCase "Fail mem reduction" $ memTestGen "((? x F (x 2) x) (3 4))" "(F 3 4)"
    , testCase "Free mem reduction" $ memTestGen "((? x F * x) *)" "[x]x"
    , testCase "Nested mem reduction" $ memTestGen "((? x F (G x) x) (G 5))" "5"
    , testCase "Double application" $
        memTestRec
          "((? x F x x) ((? y G y y) 2))"
          "2"
    , testCase "Multiple variables" $ memTestGen "((? x y F (x y) x) (3 4))" "(y 3 4)"
    , -- , testCase "Empty term match" $ memTestGen "((? x F x (G x)) ())" "()"
      testCase "Variable capture" $ memTestGen "((? x F [y](x y) x) [z](z 1))" "[z]z"
    , -- , testCase "Complex nested patterns" $ memTestGen "((? x F (G (H x)) x) (G (H 9)))" "9"
      testCase "Application reduction" $ memTestGen "((? f x (f x) f) (G 5))" "(x G 5)"
    , testCase "∀ test" memTestForall
    ]

{- |
@
 ∀ := (λ x . Λ y . x)
 ==> (ל x ! x $ (ל y ! * x) *)
@
-}
memTestForall :: Assertion
memTestForall = memTestRec "((? x ! x ((? y ! * x)) *) (y = y))" "[y](y = y)"
