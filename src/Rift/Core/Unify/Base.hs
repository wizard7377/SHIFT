{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Rift.Core.Unify.Base where

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
data FTerm a = FTerm {_term :: Term' a, _frees :: [Term' a]}

deriving instance (Eq a) => Eq (FTerm a)
deriving instance (Show (Term' a)) => Show (FTerm a)

addFrees :: (Atomic atom) => FTerm atom -> [Term' atom] -> FTerm atom
addFrees (FTerm t fs) new = FTerm t (fs <> new)
replaceTerm :: (Atomic atom) => FTerm atom -> Term' atom -> FTerm atom
replaceTerm (FTerm _ fs) new = FTerm new fs
type BindingSet a = HMap a

data UnificationEnv a = Unification
  { _binds :: BindingSet a
  , _varsUp :: [a]
  , _varsDown :: [a]
  }

data UnificationResult a = UnificationResult
  { _lowering :: BindingSet a
  , _raising :: BindingSet a
  , _upBinds :: [a]
  , _downBinds :: [a]
  }

makeLenses ''UnificationEnv
makeLenses ''UnificationResult

type UnificationAttempt a = Choice (UnificationResult a)
generate :: (Atomic atom) => (Applicative Choice) => Term' atom -> Term' atom -> Choice (BindingSet (Term' atom))
generate up down =
  pure [(up, down)]
    <|> ( case (up, down) of
            (TAtom _, _) -> mempty
            (_, TAtom _) -> mempty
            (TCons a0 a1, Cons b0 b1) -> (generate a0 b0) <> (generate a1 b1)
        )
