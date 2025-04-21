module Rift.Core.Unify.Dev where

import Extra
import Extra.Basics
import Extra.Choice
import Rift.Core.Base
import Rift.Core.Unify.Base
import Rift.Core.Unify.Unify hiding (frees)

inferUnify :: (Show (UnifyAttempt (Term atom))) => (Atomic atom) => (Show (Term atom)) => (Applicative Choice) => (Monad Choice) => Term atom -> Term atom -> [Term atom] -> [Term atom] -> UnifyChoice (Term atom)
inferUnify term0 term1 freeUp freeDown =
  "Valued"
    <?> unify
    <| ("Generated" <?> (initAttempt (LEnv freeUp freeDown) (generate ("Input 1" <?> term0) ("Input 2" <?> term1))))

introsUnify :: (Atomic atom) => (Show (Term atom)) => Term atom -> Term atom -> UnifyChoice (Term atom)
introsUnify term0 term1 =
  let qterm0 = intros term0
      qterm1 = intros term1
   in (inferUnify (qterm0 ^. term) (qterm1 ^. term) (qterm0 ^. lameds) (qterm1 ^. lameds))

unifyTest :: (Atomic atom) => (Show (Term atom)) => (Term atom, Term atom) -> UnifyChoice (Term atom)
unifyTest = uncurry introsUnify
