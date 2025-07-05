{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Sift.Ops.Simple where

import Control.Applicative (Alternative (..))
import Control.Lens (from)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.RWS qualified as M
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (Foldable (..))
import Extra
import Rift qualified
import Sift.Core.Unify
import Sift.Ops.Common

nullConvert :: Convert Bool
nullConvert t0 t1 = do
  pure (t0 == t1)
alphaConvert :: _
alphaConvert = _
unifyConvert :: [Rift.TermOf thy] -> Unify' thy
unifyConvert freeBothv t0 t1 = hoist lift (unify freeBothv t0 t1)

deltaReduce :: Redux
deltaReduce t0 = do
  env <- M.ask
  let thy = env ^. opTheory
  let mapping = mapSimple (unTagMap $ thy ^. Rift.defines)
  let t1 = transform mapping t0
  pure t1
