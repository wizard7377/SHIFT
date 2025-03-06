{-# LANGUAGE BlockArguments #-}

module Rift.Core.Funcs where

import Data.Traversable (for)
import Debug.Trace
import Extra.Choice
import Extra.List
import Rift.Core.Base
import Rift.Core.Instances ()
import Rift.Core.Unify

replace :: (Atomic atom) => Term atom -> Term atom -> Term atom -> Term atom
replace from to within =
  case within of
    _ | within == from -> to
    Cons a b -> Cons (this a) (this b)
    Rule a b -> Rule (this a) (this b)
    Lamed a b -> Lamed (this a) (this b)
    _ -> within
 where
  this = replace from to
