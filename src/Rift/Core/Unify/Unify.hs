{-# LANGUAGE TemplateHaskell #-}

module Rift.Core.Unify.Unify where

import Control.Applicative
import Control.Lens (makeLenses, (&), (<>~))
import Data.Functor.Identity (Identity)
import Data.List ((\\))
import Extra
import Extra.Basics hiding (split)
import Extra.Choice
import Extra.Map
import Extra.Tuple hiding (split)
import Rift.Core.Base hiding (frees)
import Rift.Core.Unify.Base hiding (frees)

{- | A unification.
 The upper values are the ones that need to match the lower values.
 Note that a down value that is bound is *raised*, while a up value that is bound is *lowered*
-}
unify :: (Atomic atom) => UnificationEnv atom -> Choice (UnificationResult atom)
unify env =
  case split4 (\x -> x ^. _1 `elem` env ^. varsUp) (\y -> y ^. _2 `elem` env ^. varsDown) (("Env" <?@> env) ^. binds) of
    (atoms, [], [], vars) -> "Branch 1" <?@> if miso (atoms) && miso (vars) && all (uncurry (==)) atoms then [mempty] else []
    (atoms, [], lowers, levelers) ->
      let
        uniF = mapToF lowers
        lowerVars = fst <$> lowers
        newBinds = first uniF <$> (atoms ++ levelers)
        newVars = env ^. varsUp \\ lowerVars
        e0 = binds .~ newBinds $ env
        e1 = varsUp .~ newVars $ e0
       in
        "Branch 2" <?@> (<>) <$> [UnificationResult lowers [] lowerVars []] <*> unify e1
    (atoms, raisers, lowers, levelers) ->
      let
        uniF = mapToFR raisers
        raiseVars = snd <$> raisers
        newBinds = second uniF <$> (atoms ++ lowers ++ levelers)
        newVars = env ^. varsUp \\ raiseVars
        e0 = binds .~ newBinds $ env
        e1 = varsUp .~ newVars $ e0
       in
        "Branch 3" <?@> (<>) <$> [UnificationResult [] raisers [] raiseVars] <*> unify e1
