{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift.Ops.Nun (
  -- * Overview
  -- $overview
  nunReduce,
)
where

import Extra
import Rift qualified

{- $overview
נ (nun) is the inverse of מ (mem).
While מ attempts to, for @[α*]([β*](לγδεζ)[η*]θ) ≡ ι@, find ι, נ attemps to, for the same, find ε (with known ι)

That is, מ is the inverse of נ, and vice versa, although they are not strict inverses (they may generate other results)
-}

{- | The נ reduction
 The following shall refer to @[α*]([β*](לγδεζ)[η*]θ) ≡ ι@
-}
nunReduce ::
  (Rift.Term t, Eq t, Show t, Rift.RTerm t) =>
  -- | The ל term, that is, @[β*](לγδεζ)@
  t ->
  t ->
  Choice t
nunReduce = _
