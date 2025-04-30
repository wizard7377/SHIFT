{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Sift.Solver.GenSearch where

-- TODO apart from generally making this faster, also probs should do something for parital init

import Control.Lens (Lens', lens, makeLenses, over, set, view, (%=), (&), (+=), (.=), (.~), (^.), _1, _2)
import Control.Monad (join)
import Control.Monad.Identity (Identity (..))

-- import Extra.Choice (Choice (AllOf, AnyOf, EmptyNode, Simple, Trivial))

import Control.Monad.RWS (MonadReader (..))
import Control.Monad.State
import Data.Foldable (Foldable (..))
import Data.List (intersect, singleton, subsequences)
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Data.Traversable
import Debug.Trace (traceShow, traceShowId)
import Extra
import Extra.Basics
import Extra.Choice hiding (resolve)
import Extra.Choice qualified
import Extra.List (forEach, subParts)
import Extra.Map
import Extra.Tuple
import Rift
import Rift (Atomic)
import Sift.Base (LogicEnv, LogicResult (..), SAtom (..))
import Sift.Base qualified (LogicEnv (..))
import Sift.Base qualified as Base
import Sift.Monad ()
import Sift.Monad hiding (_depth)
import Sift.Monad qualified as Sift
import Sift.Types.Generate

-- import Sift.Types.Unify

type LMGen atom a = LM (SearchState atom) a

data FTerm atom = FTerm
  { _term :: Term' atom
  , _vars :: [Term' atom]
  }
  deriving (Eq, Show)

makeLenses ''FTerm

toFTerm :: (Atomic atom) => Term' atom -> FTerm atom
toFTerm term =
  FTerm
    { _term = term
    , _vars = []
    }

-- | The state of the Search monad with sentence `sen` on token `atom`
data SearchState atom = SearchState
  { _sentences :: [FTerm atom]
  -- ^ All proven statements
  , _depth :: Int
  -- ^ Current depth to end
  , _vars :: Int
  -- ^ List of infered vars
  , _gen :: Generator
  }

sentences :: Lens' (SearchState atom) [Term' atom]
sentences = lens _sentences (\s v -> s{_sentences = v})
depth :: Lens' (SearchState atom) Int
depth = lens _depth (\s v -> s{_depth = v})
vars :: Lens' (SearchState atom) Int
vars = lens _vars (\s v -> s{_vars = v})
gen :: Lens' (SearchState atom) Generator
gen = lens _gen (\s v -> s{_gen = v})
deriving instance (Show atom) => Show (SearchState atom)

instance EnterState SearchState where
  enterState :: LogicEnv -> [Term' atom] -> SearchState atom
  enterState env sens =
    SearchState
      { _sentences = toFTerm <$> sens
      , _depth = 0
      , _vars = 0
      , _gen = Generator 0
      }

genSolve :: (Atomic atom) => Term' atom -> LMGen atom (LogicResult)
genSolve goal =
  do
    (env :: LogicEnv) <- ask
    (state :: SearchState atom) <- get
    let edepth = Sift.Base._depth env
    let stateDepth = state ^. depth
    "Env" ?>> Sift.Base._depth env
    "State" ?>> state ^. sentences
    let sens = state ^. sentences
    newVals <- sequence $ genSolve' <$> sens <*> sens
    {-
    t0 <- traverse yudGenInner sens
    "t0" ?>> t0
    t1 <- traverse yudGenOuter sens
    "t1" ?>> t1
    t2 <- traverse yudRedInner sens
    "t2" ?>> t2
    t3 <- traverse yudRedOuter sens
    "t3" ?>> t3
    let t4 = concat $ [t0] ++ [t1] ++ t2 ++ t3
    -}
    let newSens = sens ++ (concat newVals)

    sentences .= newSens
    good <- resolve goal
    "good" ?>> good
    "new sens" ?>> newSens
    depth += 1
    let res
          | good = return Solved
          | (edepth > stateDepth) =
              genSolve goal
          | otherwise = return Stopped

    res

genSolve' :: (Atomic atom) => Term' atom -> Term' atom -> LMGen atom [Term' atom]
genSolve' left right =
  let QTerm left varsLeft = "left" <?> intros left
      QTerm right varsRight = "right" <?> intros right
   in case (left, right) of
        (Rule toA fromA, Rule toB fromB) ->
          let env = LEnv varsLeft varsRight
              uchoice = unify <| (initAttempt env (generate fromA toB))
              maps = unifyMaps <$> uchoice
              rewrite = (\(mapping :: (Term' v -> Term' v, Term' v -> Term' v)) -> (rule ((mapping ^. _1) toA) ((mapping ^. _2) fromB))) <$> maps
           in (return $ ("genSolve" <?> Extra.Choice.resolve rewrite))
        _ -> return []

mem :: (Atomic atom) => Term' atom -> Term' atom -> LMGen atom [Term' atom]
mem termA termB =
  let QTerm innerA varsA = intros termA
      QTerm innerB varsB = intros termB
      env = LEnv varsA varsB
   in case (innerA, innerB) of
        (Rule toA fromA, Rule toB fromB) ->
          let uchoice = unify <| (initAttempt env (generate toB fromA))
              maps = unifyMaps <$> uchoice
              rewrite = (\(mapping :: (Term' v -> Term' v, Term' v -> Term' v)) -> (rule ((mapping ^. _2) toA) ((mapping ^. _1) fromB)))
              pairs :: Choice (Term' _) = rewrite <$> maps
           in return $
                Extra.Choice.resolve
                  pairs
        _ -> return []

resolve :: (Atomic atom) => Term' atom -> LMGen atom Bool
resolve goal = do
  sens <- gets _sentences
  solves <- traverse (resolve' goal) sens
  return $ or solves

resolve' :: (Atomic atom) => Term' atom -> Term' atom -> LMGen atom Bool
resolve' goal proven =
  let QTerm innerA varsA = intros proven
      QTerm innerB varsB = intros goal
      env = LEnv varsA varsB
      uchoice = unify <| (initAttempt env (generate proven goal))
      uchoice1 = cfilter (\x -> x ^. outs . raising == []) uchoice
   in return $ not (null (Extra.Choice.resolve uchoice1))

{-

testGen :: (Atomic atom) => (Term' atom -> LMGen atom v) -> Term' atom -> v
testGen val terms =
  let func = (val $ ("Input" <?> terms))
   in case (testLMT func []) of
        Identity (_, Right v) -> v
        _ -> error "todo"
testGen2 :: (Atomic atom) => (Term' atom -> Term' atom -> LMGen atom v) -> (Term' atom, Term' atom) -> v
testGen2 val terms =
  let func = ((uncurry val) ("Input" <?> terms))
   in case (testLMT func []) of
        Identity (_, Right v) -> v
        _ -> error "todo"

-}
