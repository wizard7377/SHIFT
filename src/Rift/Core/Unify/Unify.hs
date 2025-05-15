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
unify :: (Term term, TermLike term) => BindingSet term -> UnificationEnv term -> [UnificationResult term]
unify binds env =
  case split4 (\x -> x ^. _1 `elem` env ^. varsUp) (\y -> y ^. _2 `elem` env ^. varsDown) (binds) of
    (atoms, [], [], []) -> "Branch 0" <?@> (if all (uncurry (==)) atoms then pure simpleResult else mempty)
    (atoms, [], [], vars) -> "Branch 1" <?@> (if miso atoms && miso vars && all (uncurry (==)) atoms then pure simpleResult else mempty)
    (atoms, [], lowers, levelers) ->
      let
        uniF = mapToF lowers
        lowerVars = fst <$> lowers
        newBinds = first uniF <$> (atoms ++ levelers)
        newVars = env ^. varsUp \\ lowerVars
        e1 = varsUp .~ newVars $ env
       in
        "Branch 2" <?@> (<>) <$> pure (UnificationResult lowers [] lowerVars []) <*> unify newBinds e1
    (atoms, raisers, lowers, levelers) ->
      let
        uniF = mapToFR raisers
        raiseVars = snd <$> raisers
        newBinds = second uniF <$> (atoms ++ lowers ++ levelers)
        -- newVars = env ^. varsUp \\ raiseVars
        -- ???
        newVars = env ^. varsDown \\ raiseVars
        e1 = varsDown .~ newVars $ env
       in
        "Branch 3" <?@> (<>) <$> pure (UnificationResult [] raisers [] raiseVars) <*> unify newBinds e1
