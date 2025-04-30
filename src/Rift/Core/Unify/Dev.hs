module Rift.Core.Unify.Dev where

import Extra
import Extra.Basics
import Extra.Choice
import Rift.Core.Base
import Rift.Core.Unify.Base
import Rift.Core.Unify.Unify hiding (frees)

unifyTest :: Atomic atom => FTerm atom -> FTerm atom -> Choice (UnificationResult (Term' atom))
unifyTest (FTerm t1 fs1) (FTerm t2 fs2) =
  let
    env = Unification { _binds = [], _varsUp = fs1, _varsDown = fs2 }
    result = unify env
   in
    result >>= \r -> pure (UnificationResult (r ^. lowering) (r ^. raising) (r ^. upBinds) (r ^. downBinds))
