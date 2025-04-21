{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Sift.Solver.GenSearch where

-- TODO apart from generally making this faster, also probs should do something for parital init

import Control.Lens (makeLenses, over, set, view, (%=), (&), (.=), (.~), (^.), _1, _2)
import Control.Monad (join)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State
import Data.Foldable (Foldable (..))
import Data.List (intersect, singleton, subsequences)
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Data.Traversable
import Debug.Trace (traceShow, traceShowId)
import Extra.Basics

-- import Extra.Choice (Choice (AllOf, AnyOf, EmptyNode, Simple, Trivial))

import Control.Monad.RWS (MonadReader (..))
import Extra
import Extra.Choice
import Extra.List (forEach, subParts)
import Extra.Map
import Extra.Tuple
import Rift
import Rift (Atomic)
import Sift.Base (LogicEnv, LogicResult (..), SAtom (..))
import Sift.Base qualified (LogicEnv (..))
import Sift.Monad ()
import Sift.Monad hiding (_depth)
import Sift.Monad qualified as Sift
import Sift.Types.Generate

-- import Sift.Types.Unify

type LMGen atom a = LM (SearchState atom) a

-- | The state of the Search monad with sentence `sen` on token `atom`
data SearchState atom = SearchState
  { _sentences :: [Term atom]
  -- ^ All proven statements
  , _depth :: Maybe Int
  -- ^ Current depth to end
  , _vars :: Int
  -- ^ List of infered vars
  , _gen :: Generator
  }

makeLenses ''SearchState
instance EnterState SearchState where
  enterState :: LogicEnv -> [Term atom] -> SearchState atom
  enterState env sens =
    SearchState
      { _sentences = sens
      , _depth = Sift.Base._depth env
      , _vars = 0
      , _gen = Generator 0
      }

genSolve :: (Atomic atom) => Term atom -> LMGen atom (LogicResult ())
genSolve = do
  sens <- gets _sentences
  traverse (sens)

genSolve' :: (Atomic atom) => Term atom -> Term atom -> LMGen atom (LogicResult ())
genSolve'
he :: (Atomic atom) => LMGen atom (VTerm atom)
he = do
  heGenV <- gets _gen
  let (vatom, heGenV') = heGen heGenV
  heGenV' <- gen .= heGenV'
  return (Atom vatom)

yudGenOuter :: (Atomic atom) => Term atom -> LMGen atom (Term atom)
yudGenOuter term =
  return $ cons term Yud

yudGenInner :: (Ord atom) => (Eq atom) => (Atomic atom) => Term atom -> LMGen atom (Term atom)
yudGenInner term = do
  let QTerm inner vars = intros term
  let newTerm = unintros $ QTerm (rule inner Yud) vars
  return newTerm

yudRedOuter :: (Ord atom) => (Eq atom) => (Atomic atom) => Term atom -> LMGen atom [Term atom]
yudRedOuter (BCons Rule a Yud) = do
  return [a]
yudRedOuter _ = return []
yudRedInner :: (Ord atom) => (Eq atom) => (Atomic atom) => Term atom -> LMGen atom [Term atom]
yudRedInner term =
  let QTerm inner vars = intros term
   in case inner of
        (BCons Rule a Yud) -> do
          let newTerm = unintros $ QTerm a vars
          return [newTerm]
        _ -> return []

mem :: (Atomic atom) => Term atom -> Term atom -> LMGen atom [Term atom]
mem termA termB =
  let
    QTerm innerA varsA = intros termA
    QTerm innerB varsB = intros termB
    env = LEnv varsA varsB
   in
    case (innerA, innerB) of
      (BCons Rule toA fromA, BCons Rule toB fromB) ->
        let
          uchoice = unify <| (initAttempt env (generate toB fromA))
          maps = unifyMaps <$> uchoice
          rewrite = (\(mapping :: (Term v -> Term v, Term v -> Term v)) -> (rule ((mapping ^. _2) toA) ((mapping ^. _1) fromB)))
          pairs :: Choice (Term _) = rewrite <$> maps
         in
          return $
            Extra.Choice.resolve
              pairs
      _ -> return []

resolve :: (Atomic atom) => Term atom -> LMGen atom Bool
resolve goal = do
  sens <- gets _sentences
  solves <- traverse (resolve' goal) sens
  return $ or solves
resolve' :: (Atomic atom) => Term atom -> Term atom -> LMGen atom Bool
resolve' goal proven =
  let
    QTerm innerA varsA = intros proven
    QTerm innerB varsB = intros goal
    env = LEnv varsA varsB
    uchoice = unify <| (initAttempt env (generate proven goal))
    uchoice1 = cfilter (\x -> x ^. outs . raising == []) uchoice
   in
    return $ not (null (Extra.Choice.resolve uchoice1))
testGen :: (Atomic atom) => (Term atom -> LMGen atom v) -> Term atom -> v
testGen val terms =
  let func = (val $ ("Input" <?> terms))
   in case (testLMT func []) of
        Identity (_, Right v) -> v
        _ -> error "todo"
testGen2 :: (Atomic atom) => (Term atom -> Term atom -> LMGen atom v) -> (Term atom, Term atom) -> v
testGen2 val terms =
  let func = ((uncurry val) ("Input" <?> terms))
   in case (testLMT func []) of
        Identity (_, Right v) -> v
        _ -> error "todo"
