module Rift.Core.Generate () where

import Control.Lens (Plated, cosmos)
import Control.Lens.Combinators (universe)
import Data.Text qualified as T
import Rift.Core (addFree)
import Rift.Core.Base
import Rift.Core.Classes
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

type TestGen = Gen TestTerm

{-
-- |The type of terms used for testing `Sift`
data Term' atom where
  -- |ל
  PrimLamed :: Term' atom
  -- |ט
  PrimAtom :: atom -> Term' atom
  -- |כ
  PrimCons :: Term' atom -> Term' atom -> Term' atom
  PrimTag :: Term' atom -> Int -> Term' atom
  -- |ד
  PrimFree :: Term' atom -> [Term' atom] -> Term' atom
  -- |`λfrom . λto . λterm . (term[to := from])`
  PrimRep :: Term' atom -> Term' atom -> Term' atom -> Term' atom
  PrimError :: Term' atomgenerateTerm' :: Int -> Gen term
-}

div1 :: Int -> Int -> Int
div1 a 0 = 1
div1 a b = a `div` b
asciiChars = ['a' .. 'z']
instance Arbitrary TestToken where
  arbitrary = do
    token <- (elements asciiChars)
    number <- chooseInt (0, 9)
    pure $ TestToken $ Left $ T.pack $ token : show number

instance Arbitrary TestTerm where
  arbitrary = generateTerm
genName = resize 4 $ do
  token <- listOf1 (elements asciiChars)
  pure $ TestToken $ Left $ T.pack $ token
generateTerm' :: Int -> TestGen
generateTerm' n = scale (`div1` n) generateTerm
generateTerm :: TestGen
generateTerm = do
  siz <- getSize
  if siz <= 1
    then PrimAtom <$> genName
    else
      frequency
        [ (5, goodLamed)
        , (1, badLamed)
        , (1, replaceTest)
        , (2, freeTest)
        , (2, basicKaf)
        , (6, klist)
        , (2, PrimAtom <$> genName)
        ]

--- Things that we need to appear
-- Lamed tests
goodLamed :: TestGen
goodLamed = do
  name <- (arbitrary :: Gen TestToken)
  badcase <- scale (`div1` 3) generateTerm
  arg <- scale (`div1` 3) generateTerm
  goodcase <- scale (`div1` 3) generateTerm
  pure $ Lamed (PrimAtom name) badcase arg goodcase

badLamed :: TestGen
badLamed = pure PrimLamed

-- Kaf tests

basicKaf :: TestGen
basicKaf = do
  lhs <- generateTerm' 2
  rhs <- generateTerm' 2
  pure $ Kaf lhs rhs
klist :: TestGen
klist = do
  siz <- getSize
  len <- chooseInt (1, (siz `div1` 2))
  list <- (resize len $ listOf1 (resize (siz `div1` len) generateTerm))
  pure $ foldl1 Kaf list

-- Replace tests
replaceTest :: TestGen
replaceTest = do
  name <- genName
  to <- generateTerm' 2
  within <- generateTerm' 2
  pure $ PrimRep (PrimAtom name) to within

-- Free tests
freeTest :: TestGen
freeTest = do
  frees <- resize 2 $ listOf1 genName
  terms <- generateTerm' 2
  pure $ foldr (\term acc -> addFree acc (PrimAtom term)) terms frees

-- | Generate a term that occurs in a given term
genOccurs :: (Plated TestTerm) => TestTerm -> Gen TestTerm
genOccurs term = do
  let freeTerms = universe term
  if null freeTerms
    then generateTerm
    else elements freeTerms

genNotOccurs :: (Plated TestTerm) => TestTerm -> Gen TestTerm
genNotOccurs term = do
  let freeTerms = universe term
  if null freeTerms
    then generateTerm
    else do
      notOccurs <- generateTerm
      if poccurs notOccurs term
        then genNotOccurs term
        else pure notOccurs
