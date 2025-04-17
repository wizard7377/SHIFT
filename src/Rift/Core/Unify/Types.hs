{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Rift.Core.Unify.Types where

import Control.Lens (makeLenses)
import Control.Lens qualified as Lens
import Extra.Basics
import Extra.Choice
import Extra.Map
import Rift.Core.Base
import Rift.Core.Instances ()

{- | The basic unification function
unify :: (Eq (box tok), Ord (Term atom)) => Term atom -> Term atom -> Unification atom
-}
data QTerm atom = QTerm {_root :: Term atom, _free :: [Term atom]}

deriving instance (Eq (Term atom)) => Eq (QTerm atom)
deriving instance (Show (Term atom)) => Show (QTerm atom)

makeLenses ''QTerm

-- | The type of bindings, usually notated \(sigma\)
type Unification a = HMap a

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

{- | The solver state
 In @forall x. (T0 -> forall a. A)@ & @forall y. (forall b. B -> T1)@...
-}
data UResult atom = UResult
  { _bindingLeft :: Unification atom
  -- ^ @x@
  , _bindingRight :: Unification atom
  -- ^ @y@
  , _reflexive :: Unification atom
  , _pairing :: Unification atom
  , _unknown :: Unification atom
  }
  deriving (Eq, Ord, Functor, Foldable, Traversable)

deriving instance (Show atom, Show (Term atom)) => Show (UResult atom)
Lens.makeLenses ''UResult

applySplit :: (Atomic atom) => (Unification (Term atom), Unification (Term atom), Unification (Term atom), Unification (Term atom)) -> UResult (Term atom) -> UResult (Term atom)
applySplit (a, b, c, d) (UResult bL bR rL pR uK) = mkUResult (b <> bL) (c <> bR) (rL <> a) (pR <> d) uK

-- | A given state of a unification
type ULeaf atom = (UResult atom, UState atom)

type ULeafOld atom = (Unification atom, UState (Term atom))

-- | A list of states of a unification
type UTree atom = Choice (ULeaf atom)

class Unify a where
  unify :: a -> a -> Choice (Unification a)

simpleResult :: (Atomic atom) => Unification atom -> UResult atom
simpleResult input = UResult [] [] [] [] input
basic :: (Atomic atom) => Unification atom -> ULeaf atom
basic val = (simpleResult val, UState [] [])

mkUResult ::
  (Atomic atom) =>
  -- | Binding left
  Unification atom ->
  -- | Binding right
  Unification atom ->
  -- | Basic
  Unification atom ->
  -- | Pairing
  Unification atom ->
  -- | Unknown
  Unification atom ->
  UResult atom
mkUResult = UResult

-- | Constructor for uTree
uTree :: (Atomic atom) => Choice (Unification (Term atom)) -> [Term atom] -> [Term atom] -> UTree (Term atom)
uTree uni left right = (\x -> (simpleResult x, UState left right)) <$> uni
