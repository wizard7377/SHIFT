{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Rift.Core.Unify.Base where

import Control.Applicative (Alternative (..))
import Control.Lens (makeLenses)
import Control.Lens.Operators
import Extra.Basics
import Extra.Choice
import Extra.Map
import Rift.Core.Base

{- | A simple alias
 Note that to avoid the confusing "left" and "right" terms commenly used, the terms top (general or first) and bottom (specific or second)
 That is, it is of the form `(Top, Bottom)`.
 A "lowering" refers to the operation from `x` to `y`, with respect to `(x,y)`, as it goes from top to bottom
 A raising refers to `y` to `x`
-}
type BindingSet a = HMap a

data LEnv a = LEnv
  { _varsUp :: [a]
  , _varsDown :: [a]
  }
  deriving (Eq, Show)

makeLenses ''LEnv

generate :: (Atomic atom) => (Applicative Choice) => Term' atom -> Term' atom -> Choice (BindingSet (Term' atom))
generate up down =
  pure [(up, down)]
    <||> ( case (up, down) of
            (TAtom _, _) -> cabsurd
            (_, TAtom _) -> cabsurd
            (TCons a0 a1, TCons b0 b1) -> (generate a0 b0) <> (generate a1 b1)
         )

data QTerm a = QTerm
  { _term :: Term' a
  , _lameds :: [Term' a]
  }
  deriving (Eq)

deriving instance (Show (Term' a)) => Show (QTerm a)
makeLenses ''QTerm
intros :: (Atomic atom) => Term' atom -> QTerm atom
unintros :: (Atomic atom) => QTerm atom -> Term' atom
intros term = case term of
  Lamed (termToList -> vars) t -> QTerm t vars
  _ -> QTerm term []

unintros term =
  case term of
    QTerm t [] -> t
    QTerm t (listToTerm -> Just vars) -> lamed vars t
    _ -> undefined -- TODO

{-
genIntros :: (Atomic atom) => Term' atom -> Choice (QTerm atom)
genIntros term =
  (pure $ QTerm term [])
    <|> ( case term of
            Lamed b t -> over lameds (b :) <$> (genIntros t)
            _ -> cabsurd
        )
-}
