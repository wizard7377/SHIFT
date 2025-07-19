{-# LANGUAGE PartialTypeSignatures #-}

module Tests.Unify where

import Control.Lens qualified as Lens
import Control.Lens.Operators ((^.))
import Control.Monad.State
import Data.Choice (Choice)
import Data.Foldable (Foldable (..))
import Data.List (nubBy)
import Data.List.Extra (splitOn)
import Extra.Map.Other (equivalent)
import Extra.Parsers
import Rift hiding (Assertion)
import Rift.Core.Generate ()
import Sift (unify, unifyGraph)
import Sift.Core (UnifyState)
import Sift.Core.Dev.Util
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC
import Tests.Common

unifyTestMsg ::
  Choice (UnifyState TestTerm) ->
  Int ->
  [TestTerm] ->
  TestTerm ->
  TestTerm ->
  String
unifyTestMsg res num vars termA termB = (show res ++ " does not have length " ++ show num ++ " (actually has length " ++ show (length (toList res)) ++ ") ; (trying to unify \"" ++ show termA ++ "\" and \"" ++ show termB ++ "\" with [" ++ show vars ++ "])")

-- | Create a unification test case
unifyTestGen ::
  (Lens.Plated Rift.TestTerm) =>
  -- | The expected outputs
  Int ->
  -- | The shared variables
  String ->
  -- | The first term
  String ->
  -- | The second term
  String ->
  Assertion
unifyTestGen num vars' termA' termB' = do
  vars <- if vars' == "" then pure [] else traverse tRead (splitOn ";;" vars')
  termA <- tRead termA'
  termB <- tRead termB'
  let res0 = unify vars termA termB
  assertBool (unifyTestMsg res0 num vars termA termB) ((length $ toList res0) == num)

unifyTests :: (Lens.Plated Rift.TestTerm) => TestTree
unifyTests = testGroup "Unify tests" [unifyUnitTests, unifyPropTests]
unifyUnitTests :: (Lens.Plated Rift.TestTerm) => TestTree
unifyUnitTests =
  testGroup
    "Unify unit tests"
    [ testCase "Simple variable unification" $
        unifyTestGen 1 "X" "X" "a"
    , testCase "Another variable unification" $
        unifyTestGen 1 "" "[Y]Y" "b"
    , testCase "Two variables with same term" $
        unifyTestGen 2 "X;;Y" "(f X)" "(f Y)"
    , testCase "Function symbol mismatch" $
        unifyTestGen 0 "X" "(f X)" "(g X)"
    , testCase "Complex term unification" $
        unifyTestGen 1 "X;;Y" "(f X (g Y))" "(f (h Z) (g W))"
    , testCase "Recursive structure" $
        unifyTestGen 1 "X" "(f X)" "(f (f a))"
    , testCase "Recursive structures 2" $
        unifyTestGen 1 "X" "X" "(f X)"
    , -- , testCase "Multiple valid substitutions" $
      --    unifyTestGen 5 "X;;Y;;Z" "(f X Y)" "(f Z Z)"
      testCase "Constant unification" $
        unifyTestGen 1 "" "a" "a"
    , testCase "Constants don't unify" $
        unifyTestGen 0 "" "a" "b"
    , testCase "Nested terms with multiple variables" $
        unifyTestGen 1 "X;;Y;;Z" "(f X (g Y Z))" "(f a (g b c))"
    , testCase "Unification with shared variables" $
        unifyTestGen 1 "X" "(f X X)" "(f a a)"
    , testCase "Failed unification with shared variables" $
        unifyTestGen 0 "X" "(f X X)" "(f a b)"
    , testCase "Unification failed with shared and not shared" $
        unifyTestGen 0 "X;;Z" "[Y](f X X a Y)" "(f Z b Z c)"
    , testCase "Shared variables only once" $
        unifyTestGen 0 "X" "[C](f X a C)" "[D](f b X D)"
    , testCase "Compound nested structures" $
        unifyTestGen 1 "X;;Y;;Z" "(f (g X) (h Y Z))" "(f (g a) (h b c))"
    ]

generateTerm :: QC.Gen TestTerm
generateTerm = QC.arbitrary
unifyPropTests :: (Lens.Plated Rift.TestTerm) => TestTree
unifyPropTests =
  testGroup
    "Unify property tests"
    []

-- [QC.testProperty "Unify same" $ QC.forAll generateTerm (\x -> not . null $ toList $ unify [] x x)]
