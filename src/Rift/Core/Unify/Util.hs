{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Core.Unify.Util where

import Data.Maybe (catMaybes)
import Extra
import Rift.Core.Base
import Rift.Core.Unify.Base
import Rift.Core.Unify.Infer

class (Term t) => FTermLike m t | m -> t where
  fterm :: Lens' m (t)
  ffrees :: Lens' m [(t)]

instance (Term t) => FTermLike (t, [t]) t where
  fterm = _1
  ffrees = _2

data FTerm t = FTerm
  { _term :: t
  , _frees :: [t]
  }
  deriving (Eq, Show)

makeLenses ''FTerm
instance (Term t) => FTermLike (FTerm t) t where
  fterm = term
  ffrees = frees

unify :: forall t t1 u. (FTermLike t u, FTermLike t1 u, TermLike u, Term u) => t -> t1 -> [UnifyState u]
unify t0 t1 = runChoice $ unifyInfer (t0 ^. fterm) (t1 ^. fterm) (UnifyState (Free <$> (t0 ^. ffrees)) (Free <$> (t1 ^. ffrees)))

getBound :: [TermState t] -> [t]
getBound l = catMaybes $ map (\case Bound x _ -> Just x; _ -> Nothing) l
getFree l = catMaybes $ map (\case Free x -> Just x; _ -> Nothing) l
