{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Core.Interface where

import Data.Type.Equality ((:~:) (..))
import Extra
import Rift.Core.Base (KTerm, TestTerm, poccurs)

-- | The class of all terms that have a list of variables within them
class FTerm m where
  -- | The type of the term contained within
  type Inner m :: Type

  -- | The inner term
  fterm :: (KTerm (Inner m)) => Lens' m (Inner m)

  -- | The variables
  frees :: (KTerm (Inner m)) => Lens' m [(Inner m)]

  ffrees :: (KTerm (Inner m)) => Lens' m [Inner m]
  ffrees = frees
  groundTerm :: Inner m -> m
  {-# MINIMAL fterm, frees, groundTerm #-}

{-# DEPRECATED ffrees "Use frees instead" #-}
class UTerm tag term | term -> tag where
  uniqueCreate :: term -> tag -> term

class RTerm term where
  replaceTerm :: term -> term -> (term -> term)
instance FTerm (t, [t]) where
  type Inner (t, [t]) = t
  fterm = _1
  frees = _2
  groundTerm t = (t, [])
data FTerm' t = FTerm'
  { _ftterm :: t
  , _ftfrees :: [t]
  }
  deriving (Eq, Show)

makeLenses ''FTerm'
instance FTerm (FTerm' t) where
  type Inner (FTerm' t) = t
  fterm = ftterm
  frees = ftfrees
  groundTerm t = FTerm' t []

fInnerEq :: (FTerm t0, FTerm t1, t0 ~ t1) => (Inner t0 :~: Inner t1)
fInnerEq = Refl
simplifyF ::
  (FTerm t, KTerm (Inner t), Eq (Inner t)) =>
  t ->
  t
simplifyF t =
  let
    inner = t ^. fterm
    frees = t ^. ffrees
    frees1 = filter (poccurs inner) frees
   in
    t & ffrees .~ frees1

type FullTermLike tag term =
  ( KTerm term
  , FTerm term
  , UTerm tag term
  -- , RTerm term
  )

type Term term = FullTermLike Idx term
