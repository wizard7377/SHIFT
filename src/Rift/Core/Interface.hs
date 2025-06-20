{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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

class ETerm term where
  eterm :: term

-- | Inner, var
addFree :: (FTerm term, KTerm (Inner term)) => term -> Inner term -> term
addFree t inner =
  let
    frees' = t ^. frees
   in
    t & frees .~ (inner : frees')

-- | Inner, var
addFrees :: (FTerm term, KTerm (Inner term)) => term -> [Inner term] -> term
addFrees t inner =
  let
    frees' = t ^. frees
   in
    t & frees .~ (inner <> frees')

freeTerm :: (FTerm term, KTerm (Inner term)) => term -> ([Inner term], Inner term)
freeTerm t =
  (t ^. frees, t ^. fterm)

boundTerm :: forall term. (FTerm term, KTerm (Inner term)) => [Inner term] -> Inner term -> term
boundTerm free inner =
  let
    (t :: term) = groundTerm inner
   in
    t & frees .~ free
pattern FreeTerm :: (FTerm term, KTerm (Inner term)) => [Inner term] -> Inner term -> term
pattern FreeTerm frees inner <- (freeTerm -> (frees, inner))
  where
    FreeTerm frees inner = boundTerm frees inner
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

class
  ( KTerm term
  , FTerm term
  , UTerm tag term
  , Inner term ~ term
  -- , RTerm term
  ) =>
  FullTermLike tag term
instance
  ( KTerm term
  , FTerm term
  , UTerm tag term
  , Inner term ~ term
  -- , RTerm term
  ) =>
  FullTermLike tag term

class (FullTermLike Idx term) => Term term
instance (FullTermLike Idx term) => Term term
termEq :: (Term term) => term :~: (Inner term)
termEq = Refl
