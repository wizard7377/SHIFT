{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Core.Unify.Util where

import Data.Maybe (catMaybes)
import Extra
import Rift.Core.Base
import Rift.Core.Interface (FTermLike (..))
import Rift.Core.Unify.Base
import Rift.Core.Unify.Infer

unify ::
  forall t0.
  (TermLike (Inner t0), FTermLike t0, Term (Inner t0)) =>
  forall t1.
  (TermLike (Inner t1), FTermLike t1, Term (Inner t1)) =>
  forall i.
  (i ~ (Inner t0), i ~ (Inner t1)) =>
  t0 ->
  t1 ->
  Choice (UnifyState i)
unify t0 t1 = unifyInfer (t0 ^. fterm) (t1 ^. fterm) (UnifyState (Free <$> (t0 ^. ffrees)) (Free <$> (t1 ^. ffrees)))

getBound :: [TermState t] -> [t]
getBound l = catMaybes $ map (\case Bound x _ -> Just x; _ -> Nothing) l
getFree l = catMaybes $ map (\case Free x -> Just x; _ -> Nothing) l
