{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuantifiedConstraints #-}

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
  (AnyTerm term, TermLike term) =>
  -- | The term to look for
  term ->
  -- | The value to replace with
  term ->
  -- | The actual term being searched
  term ->
  term
replace from to within =
  case within of
    _ | within == from -> to
    Cons a b -> cons (this a) (this b)
    _ -> within
 where
  this = replace from to

replaceAll :: (AnyTerm term, TermLike term) => HMap term -> term -> term
replaceAll mapping term = foldr (uncurry replace) term mapping
