{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Sift.Ops.Simple where

import Control.Applicative (Alternative (..))
import Control.Lens (from)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.RWS qualified as M
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (Foldable (..))
import Data.List (permutations)
import Extra
import Rift qualified
import Short
import Sift.Core.Types
import Sift.Core.Unify
import Sift.Ops.Common

-- TODO:
alphaConvert :: Convert e
alphaConvert t0 t1 = empty

-- orderReduce :: Redux e
orderReduce (Rift.FreeTerm vars term) = do
  vars' <- csplit $ pure $ permutations vars
  pure $ Rift.FreeTerm vars' term

deltaReduce :: (Rift.Theory e) => (TOC e) => Reduce e
deltaReduce t0 = do
  state <- lift $ lift M.get
  let thy = state ^. opTheory
  let mapping = mapSimple (unTagMap $ thy ^. Rift.defines)
  let t1 = transform mapping t0
  if t0 == t1 then empty else pure t1
