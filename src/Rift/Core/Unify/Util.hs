{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Core.Unify.Util where

import Extra
import Rift.Core.Base
import Rift.Core.Unify.Base
import Rift.Core.Unify.Infer

class (Term t) => FTermLike m t | m -> t where
  term :: Lens' m (t)
  frees :: Lens' m [(t)]

instance (Term t) => FTermLike (t, [t]) t where
  term = _1
  frees = _2

data FTerm t = FTerm
  { _fterm :: t
  , _ffrees :: [t]
  }
  deriving (Eq, Show)

makeLenses ''FTerm
instance (Term t) => FTermLike (FTerm t) t where
  term = fterm
  frees = ffrees

unify :: forall t {u}. (FTermLike t u, Eq (u)) => t -> t -> [UnifyState u]
unify a b = snd <$> (runUnifyM (unifyInfer (a ^. term) (b ^. term)) $ UnifyState (a ^. frees) (b ^. frees) (UnifyResult mempty mempty))
