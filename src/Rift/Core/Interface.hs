{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Core.Interface where

import Data.Type.Equality ((:~:) (..))
import Extra
import Rift.Core.Base (Term, TestTerm)

-- | The class of all terms that have a list of variables within them
class FTermLike m where
  -- | The type of the term contained within
  type Inner m :: Type

  -- | The inner term
  fterm :: (Term (Inner m)) => Lens' m (Inner m)

  -- | The variables
  ffrees :: (Term (Inner m)) => Lens' m [(Inner m)]

class UTermLike term tag | term -> tag where
  uniqueCreate :: term -> tag -> term

instance (Term t) => FTermLike (t, [t]) where
  type Inner (t, [t]) = t
  fterm = _1
  ffrees = _2
data FTerm t = FTerm
  { _term :: t
  , _frees :: [t]
  }
  deriving (Eq, Show)

makeLenses ''FTerm
instance (Term t) => FTermLike (FTerm t) where
  type Inner (FTerm t) = t
  fterm = term
  ffrees = frees

fInnerEq :: (FTermLike t0, FTermLike t1, t0 ~ t1) => (Inner t0 :~: Inner t1)
fInnerEq = Refl
