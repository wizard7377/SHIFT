{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Rift.Core.UnifyOld.Base where

import Control.Applicative (Alternative (..))
import Control.Lens (makeLenses)
import Control.Lens.Operators
import Data.Functor.Identity (Identity)
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

generate :: (Atomic atom, Alternative Identity) => (Applicative Choice) => Term' atom -> Term' atom -> Choice (BindingSet (Term' atom))
generate up down =
  pure [(up, down)]
    <||> ( case (up, down) of
            (TAtom _, _) -> mempty
            (_, TAtom _) -> mempty
            (TCons a0 a1, Cons b0 b1) -> (generate a0 b0) <> (generate a1 b1)
         )

data QStep a = QStep
  { _preq :: Term' a
  , _fvar :: Term' a
  }

makeLenses ''QStep

data QTerm a = QTerm
  { _term :: Term' a
  , _lameds :: [(Term' a, Term' a)]
  }
  deriving (Eq)

deriving instance (Show (Term' a)) => Show (QTerm a)

makeLenses ''QTerm

intros :: (Atomic atom) => Term' atom -> Choice (QTerm atom)
unintros :: (Atomic atom) => QTerm atom -> Choice (Term' atom)
intros term =
  (pure (QTerm term [])) <|> case term of
    Lamed v b t -> lameds <>~ (intros b) (QStep t v)
    _ -> _

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
