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

unify :: (Atomic atom) => UnificationEnv atom -> Choice (UnificationResult atom)
unify env =
  case split4 (\x -> x ^. _1 `elem` env ^. varsUp) (\y -> y ^. _2 `elem` env ^. varsDown) (env ^. binds) of
    (atoms, [], [], vars) -> if (miso atoms) then pure (UnificationResult [] [] [] []) else cabsurd
    (atoms, lowers, [], levelers) ->
      let
        uniF = mapToF lowers
        lowerVars = fst <$> lowers
        newBinds = first uniF <$> (atoms ++ levelers)
        newVars = env ^. varsUp \\ lowerVars
        e0 = binds .~ newBinds $ env
        e1 = varsUp .~ newVars $ e0
       in
        unify e1
    (atoms, lowers, raisers, levelers) ->
      let
        uniF = mapToFR raisers
        raiseVars = snd <$> raisers
        newBinds = second uniF <$> (atoms ++ lowers ++ levelers)
        newVars = env ^. varsUp \\ raiseVars
        e0 = binds .~ newBinds $ env
        e1 = varsUp .~ newVars $ e0
       in
        unify e1
