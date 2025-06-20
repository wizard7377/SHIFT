module Sift.Core.Ops.Mem where

import Control.Applicative (Alternative (..))
import Data.Foldable (toList)
import Extra
import Rift qualified
import Sift.Core.Unify

{- | The mem reduce function performs mem reduction
-- That is, it takes a term of the form:
-- `<α...>(<β...>(לγδεζ)<η...>θ)`
-- α, β, η are lists of terms, γ, δ, ε, ζ, and θ are terms
--
-- The semantics are as follows:
-- if ε can unify with θ by binding β and γ in δ,
-- η in θ,
-- α varying simulataneously in both
-- then resolve this to be, given a bind
-}
memReduce :: (Rift.Term t, Eq t, Show t, Rift.RTerm t) => t -> Choice t
memReduce (Rift.FreeTerm freeBothv (Rift.Kaf (Rift.FreeTerm freeLeftv (Rift.Lamed var bad arg ans)) input)) = do
  binds <- unify freeBothv (Rift.addFrees arg (var : freeLeftv)) input
  bindMap <- fromList $ toList $ normalMap (binds ^. unifyGraph)
  let frees = (binds ^. freeLeft) <> (binds ^. freeRight) <> (binds ^. freeBoth)
  let newTerm = foldr (\(_, k, v) -> Rift.replaceTerm k v) ans (bindMap ^. seeMapTup)
  pure newTerm
memReduce _ = empty
