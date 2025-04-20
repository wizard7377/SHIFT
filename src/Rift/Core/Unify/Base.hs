{-# LANGUAGE TemplateHaskell #-}

module Rift.Core.Unify.Base where

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

generate :: (Atomic atom) => (Applicative Choice) => Term atom -> Term atom -> Choice (BindingSet (Term atom))
generate up down =
  pure [(up, down)]
    <||> pure
      ( case (up, down) of
          (Yud, Yud) -> []
          (Empty, Empty) -> []
          (Rule f0 t0, Rule f1 t1) -> [(f0, f1), (t0, t1)]
          (Lamed b0 t0, Lamed b1 t1) -> [(b0, b1), (t0, t1)]
          (Cons a0 b0, Cons a1 b1) -> [(a0, a1), (b0, b1)]
          _ -> []
      )

data QTerm a = QTerm
  { _term :: Term a
  , _lameds :: [Term a]
  }

makeLenses ''QTerm
intros :: (Atomic atom) => Term atom -> QTerm atom
unintros :: (Atomic atom) => QTerm atom -> Term atom
intros term = case term of
  Lamed b t -> over lameds (b :) (intros t)
  _ -> QTerm term []

unintros term =
  case term of
    QTerm t [] -> t
    QTerm t (b : bs) -> Lamed b (unintros (QTerm t bs))
