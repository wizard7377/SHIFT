{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Rift.Core.Unify.Types where

import Control.Lens (makeLenses)
import Control.Lens qualified as Lens
import Extra.Basics
import Extra.Choice
import Extra.Map
import Rift.Core.Base

{- | The basic unification function
unify :: (Eq (box tok), Ord (Term atom)) => Term atom -> Term atom -> Unification atom
-}
data QTerm atom = QTerm {_root :: Term atom, _free :: [Term atom]}
  deriving (Eq)

deriving instance (Show (Term atom)) => Show (QTerm atom)

makeLenses ''QTerm

qtMap :: (Term atom -> Term atom) -> QTerm atom -> QTerm atom
qtMap f t = t Lens.& root %~ f

mkQTerm :: Term atom -> [Term atom] -> QTerm atom
mkQTerm = QTerm
unQTerm :: QTerm atom -> (Term atom, [Term atom])
unQTerm (QTerm t f) = (t, f)

{- | The type of all unification, takes a list of bindings, then a list of free terms
 Note that the unification functions go from general to specific, so @unify t0 t1@ is saying that t0 is more general then t1
-}

{- | The solver state
 In @forall x. (T0 -> forall a. A)@ & @forall y. (forall b. B -> T1)@...
-}
data UState part = UState
  { _freeLeft :: [part]
  -- ^ @x@
  , _freeRight :: [part]
  -- ^ @y@
  }
  deriving (Eq, Ord, Functor, Foldable, Traversable)

deriving instance (Show atom) => Show (UState atom)
Lens.makeLenses ''UState

-- | The type of bindings, usually notated \(sigma\)
type Unification a = HMap a

-- | A given state of a unification
type ULeaf atom = (Unification (Term atom), UState (Term atom))

-- | A list of states of a unification
type UTree atom = Choice (ULeaf atom)

class Unify a where
  unify :: a -> a -> Choice (Unification a)
