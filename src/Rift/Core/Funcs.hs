{-# LANGUAGE BlockArguments #-}

module Rift.Core.Funcs where

import Data.Traversable (for)
import Debug.Trace
import Extra.Choice
import Extra.List
import Extra.Map
import Rift.Core.Base
import Rift.Core.Instances ()
import Rift.Core.Unify

{- | Replace every instance of one term with another
 Note that this is _not_ the same thing as `<$>` as using `<$>` would only replace _atoms_ not complex terms
-}
replace ::
  (Atomic atom) =>
  -- | The term to look for
  Term atom ->
  -- | The value to replace with
  Term atom ->
  -- | The actual term being searched
  Term atom ->
  Term atom
replace from to within =
  case within of
    _ | within == from -> to
    BCons Cons a b -> BCons Cons (this a) (this b)
    BCons Rule a b -> BCons Rule (this a) (this b)
    BCons Lamed a b -> BCons Lamed (this a) (this b)
    _ -> within
 where
  this = replace from to

replaceAll :: (Atomic atom) => HMap (Term atom) -> Term atom -> Term atom
replaceAll mapping term = foldr (\(from, to) -> replace from to) term mapping
