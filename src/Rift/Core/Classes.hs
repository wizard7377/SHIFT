{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Rift.Core.Classes
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Rift.Core.Classes (PrimDalet (..), PrimKaf (..), PrimAyin (..), FTerm (..), KTerm (..), AnyTerm, RTerm (..), UTerm (..), pattern Ayin) where

import Control.Lens qualified as Lens
import Extra

-- | ד := λ free . λ term . (∀free . term)
data PrimDalet term = PrimDaletCon term term

data PrimKaf term = PrimKafCon term term
data PrimAyin term = PrimAyinCon term term term
pattern Ayin :: (KTerm term, RTerm term) => term -> term -> term -> term
pattern Ayin x y z <- (preview pAyin -> Just (PrimAyinCon x y z))
  where
    Ayin x y z = review pAyin $ PrimAyinCon x y z

-- | The class of all terms that have a list of variables within them
class FTerm term where
  -- | The type of the term contained within
  type Inner term :: Type

  -- | The inner term
  fterm :: (KTerm (Inner term)) => Lens' term (Inner term)

  -- | The variables
  frees :: (KTerm (Inner term)) => Lens' term [(Inner term)]

  ffrees :: (KTerm (Inner term)) => Lens' term [Inner term]
  ffrees = frees
  groundTerm :: Inner term -> term

{-# DEPRECATED ffrees "Use frees instead" #-}
class PTerm term where
  -- | Remove one level of ף, that is, ףα ==> α[פ := ףα]
  fixed :: term -> term
class UTerm tag term | term -> tag where
  uniqueCreate :: term -> tag -> term

class RTerm term where
  pAyin :: Prism' term (PrimAyin term)
  replaceTerm :: term -> term -> (term -> term)
  replaceTerm x y = \term -> term & pAyin .~ (PrimAyinCon x y term)
  {-# MINIMAL pAyin #-}

{- | The basic class of terms
 - Fundementally, each term is something that has a way to plate to and from a nullary ל constructor, a unary ת constructor, and binary כ constructor
 -
-}
class KTerm term where
  pKaf :: Prism' term (PrimKaf term)
  isLamed :: term -> Bool
  mkLamed :: term

type AnyTerm term = KTerm term
