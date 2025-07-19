{-# LANGUAGE UndecidableInstances #-}

module Sift.Search (module Sift.Search.Reduce, solve, LogicResult (..)) where

import Data.Choice
import Data.Default
import Extra
import Rift qualified
import Short
import Sift.Core.Monad
import Sift.Core.Types
import Sift.Search.Reduce

data LogicResult e = LogicResult
  { _global :: Global e
  , _choices :: [(TO e, Local e)]
  , _solved :: Bool
  }

deriving instance (Show e, Show (TO e)) => Show (LogicResult e)
solve :: forall e. (Rift.Theory e, TOC e) => Rift.LogicEnv -> e -> TO e -> TO e -> LogicResult e
solve e thy t0 t1 =
  let (paths, global) = "TESTING" ?> runLogic (convertRec t0 t1) e thy
   in LogicResult global paths $ (any ((\(v, l) -> v == t1 && (null $ l ^. goals))) paths)
