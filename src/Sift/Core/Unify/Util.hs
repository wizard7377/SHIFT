module Sift.Core.Unify.Util where

import Data.Graph.Inductive (Graph, isSimple, mkNode)
import Extra
import Extra.Graphic

freePower :: (Eq term) => [term] -> term -> Int
freePower free t = length $ filter (== t) free
boundRoot :: (Eq term, Graph gr, Ord term) => Graphic gr term b -> term -> Int
boundRoot graphic t =
  let
    ends = graphic ^. nodes . mAt t
   in
    length ends
nonCyclic :: (Graph gr) => Choice (gr a b) -> Choice (gr a b)
nonCyclic = cfilter isSimple
