{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Sift.Core.Unify.Base where

import Control.Applicative (Alternative (..))
import Extra
import Rift qualified

data Chance
  = Simple
  | Absurd
  | Possible
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

data Unification t
  = Unification
  { _bindingUp :: HMap t
  , _bindingDown :: HMap t
  , _freeUp :: [t]
  , _freeDown :: [t]
  }
  deriving (Show, Eq, Ord, Data, Typeable, Generic)
type BindingSet t = HMap t
generateWith :: forall term. (Rift.Term term) => (term -> term -> Chance) -> term -> term -> Choice (HMap term)
generateWith f t1 t2 =
  let
    basis = f t1 t2
    basisC = case basis of
      Simple -> mempty
      Absurd -> ctrivial
      Possible -> pure $ toMap $ pure $ (,) t1 t2
   in
    basisC
      <|> ( case (t1, t2) of
              (Rift.Kaf ta1 tb1, Rift.Kaf ta2 tb2) -> (generateWith f ta1 ta2) <> (generateWith f tb1 tb2)
              _ -> cabsurd
          )

generate :: (Rift.Term term) => term -> term -> Choice (HMap term)
generate = generateWith (\_ _ -> Possible)

unify :: (Rift.Term term) => term -> term -> Choice (Unification term)
unify = _
