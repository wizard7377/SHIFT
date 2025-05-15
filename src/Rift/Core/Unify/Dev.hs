module Rift.Core.Unify.Dev where

import Data.Maybe (isJust)
import Extra
import Extra.Basics
import Extra.Choice
import Rift.Core.Base
import Rift.Core.Instances ()
import Rift.Core.Unify.Base
import Rift.Core.Unify.Unify hiding (frees)

type FTerm term = (term, [term])
unifyTest :: (Ord term, Show term, Term term) => FTerm term -> FTerm term -> [UnificationResult (term)]
unifyTest (t1, fs1) (t2, fs2) =
  let
    env = "Unify input" <?@> (Unification{_varsUp = fs1, _varsDown = fs2})
   in
    -- result = unify (generate t1 t2) env

    _

-- "Unify output" <?@> (\r -> UnificationResult (r ^. lowering) (r ^. raising) (r ^. upBinds) (r ^. downBinds)) <$> filter isJust result
