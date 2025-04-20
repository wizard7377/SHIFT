{-# LANGUAGE TemplateHaskell #-}

module Rift.Core.Unify.Unify (UnifyAttempt, binds, outs, frees, UnifyChoice, unify, unifyStep, initAttempt) where

import Control.Lens (makeLenses, (&), (<>~))
import Data.List ((\\))
import Extra.Basics
import Extra.Choice
import Extra.Map
import Extra.Tuple
import Rift.Core.Base hiding (frees)
import Rift.Core.Unify.Base hiding (frees)

data UnifyResult a = UnifyResult
  { _lowering :: BindingSet a
  , _raising :: BindingSet a
  }

deriving instance (Eq a) => Eq (UnifyResult a)
deriving instance (Show a) => Show (UnifyResult a)
data UnifyAttempt a = UnifyAttempt
  { _binds :: BindingSet a
  , _outs :: UnifyResult a
  , _frees :: LEnv a
  }

deriving instance (Eq a) => Eq (UnifyAttempt a)
deriving instance (Show a) => Show (UnifyAttempt a)
makeLenses ''UnifyResult
makeLenses ''UnifyAttempt

type UnifyChoice a = Choice (UnifyAttempt a)
initAttempt :: (Atomic atom) => (Functor Choice) => LEnv (Term atom) -> Choice (BindingSet (Term atom)) -> UnifyChoice (Term atom)
initAttempt lenv choice = (\binds -> (UnifyAttempt binds (UnifyResult [] []) lenv)) <$> choice
unify :: (Show (Term atom)) => (Atomic atom) => UnifyAttempt (Term atom) -> UnifyChoice (Term atom)
unify attempt = "Unify2" <?> cfilter verifyAttempt $ ("Unify1" <?> unifyStep attempt)

unifyStep :: (Show (Term atom)) => (Atomic atom) => UnifyAttempt (Term atom) -> UnifyChoice (Term atom)
unifyStep attempt =
  let
    upVars = attempt ^. frees . varsUp
    downVars = attempt ^. frees . varsDown
    bindst = attempt ^. binds
    outst = attempt ^. outs
   in
    case (split4 (\x -> (fst x `elem` upVars)) (\x -> (snd x `elem` downVars)) bindst) of
      (atomics, [], [], []) -> if (verifyAtoms atomics) then pure (set binds [] attempt) else cabsurd
      (atomics, [], [], vars) -> cabsurd -- TODO
      -- `(% ~ %, % ~ $, $ ~ %, $ ~ $`
      (atomics, raisers, lowers, vars) ->
        let
          lowerVars = getKeys lowers
          raiseVars = getValues raisers
          attempt1 = over (frees . varsUp) (\\ lowerVars) attempt
          attempt2 = over (frees . varsDown) (\\ raiseVars) attempt1
          attempt3 = over (frees . varsUp) (\\ lowerVars) attempt2
          attempt4 = over (frees . varsUp) (\\ raiseVars) attempt3
          attempt5 = attempt4 & (outs . lowering) <>~ lowers
          attempt6 = attempt5 & (outs . raising) <>~ raisers
          mapRaise = mapToFR (attempt6 ^. outs . raising)
          mapLower = mapToF (attempt6 ^. outs . lowering)
          attempt7 = set binds ((bindst \\ lowers) \\ raisers) attempt6
          res = over binds (fmap $ bimap mapLower mapRaise) attempt7
         in
          if (verifyAtoms atomics) then unify res else cabsurd

verifyAttempt :: (Atomic atom) => UnifyAttempt (Term atom) -> Bool
verifyAttempt attempt = minject (attempt ^. outs . lowering) && msuject (attempt ^. outs . raising) -- TODO
verifyAtoms :: (Atomic atom) => BindingSet (Term atom) -> Bool
verifyAtoms attempt = all (\(x, y) -> (x == y)) attempt

verifyBinds :: (Atomic atom) => BindingSet (Term atom) -> Bool
verifyBinds attempt = minject attempt
verifyVars :: (Atomic atom) => BindingSet (Term atom) -> Bool
verifyVars attempt = iso attempt
