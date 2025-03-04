{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Sift.Solver.GenSearch where

import Control.Monad (join)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.Foldable (Foldable (..))
import Data.List (singleton)
import Data.Maybe (catMaybes, isJust, maybeToList)
import Data.Traversable
import Debug.Trace (traceShow, traceShowId)
import Extra.Basics
import Extra.Choice (Choice (AllOf, AnyOf, EmptyNode, Simple, Trivial))
import Extra.List (forEach)
import Rift
import Rift (Atomic)
import Sift (LogicResult (..))
import Sift.Base (LogicEnv, SAtom (..), STerm, fromSTerm, qbinds, qterm, toSTerm)
import Sift.Base qualified (LogicEnv (..))
import Sift.Monad ()
import Sift.Monad hiding (_depth)
import Sift.Monad qualified as Sift
import Sift.Types.Unify

-- | The state of the Search monad with sentence `sen` on token `atom`
data SearchState sen atom = SearchState
  { _sentences :: [sen atom]
  -- ^ All proven statements
  , _depth :: Maybe Int
  -- ^ Current depth to end
  , _vars :: Int
  -- ^ List of infered vars
  }

instance EnterState SearchState Term where
  enterState :: LogicEnv -> [sen atom] -> SearchState sen atom
  enterState env sens =
    SearchState
      { _sentences = sens
      , _depth = Sift.Base._depth env
      , _vars = 0
      }

-- | Get the list of solved terms
getTerms :: (Sentence sen Term, Atomic atom) => LMGen sen atom [STerm atom]
getTerms = do
  state <- get
  let sens = _sentences state
  let val = toSTerm . toTerm <$> sens
  return val

-- | Set the list of solved terms
setTerms :: forall sen atom. (Sentence sen Term, Atomic atom) => [STerm atom] -> LMGen sen atom ()
setTerms input = modify $ \s -> s{_sentences = fromTerm <$> fromSTerm <$> input}

-- | Add some terms to the list of solved terms
addTerms :: forall sen atom. (Sentence sen Term, Atomic atom) => [STerm atom] -> LMGen sen atom ()
addTerms add = do
  terms <- getTerms
  setTerms (add ++ terms)

getDepth :: (Sentence sen Term, Atomic atom) => LMGen sen atom (Maybe Int)
getDepth = do
  state <- get
  let dep = _depth state
  return dep
setDepth :: (Sentence sen Term, Atomic atom) => Maybe Int -> LMGen sen atom ()
setDepth dep = modify $ \s -> s{_depth = dep}

-- | Can the goal be solved for (by no steps) from the given
doesSolve ::
  (Sentence sen Term, Atomic atom) =>
  -- | Given
  STerm atom ->
  -- | Goal
  STerm atom ->
  LMGen sen atom Bool
doesSolve given goal =
  let
    (givenT, givenV) = intros given
    (goalT, goalV) = intros goal
   in
    case goalT of
      _ | allowed givenV (givenT >?> goalT) -> return True

type PosSearch a = Choice (STerm a)

-- TODO?
type LMGen sen atom a = (Atomic atom) => LM (SearchState sen atom) a

-- type SearchAction a = Atomic a => STerm a -> LMGen a Bool

yudGen :: (Sentence sen Term) => STerm atom -> LMGen sen atom [Term atom]
yudGen val =
  return $
    ( let (term, vars) = intros val
       in fromTerm <$> unintros ((Rule term Yud), vars)
    )
      : (fromTerm (Rule val Yud))
      : []

yudRed :: (Sentence sen Term) => STerm atom -> LMGen sen atom [Term atom]
yudRed val =
  return $
    let (term, vars) = intros val
     in case term of
          (Rule term1 Yud) -> [unintros term1 vars]

yudSolve :: (Sentence sen Term, Atomic atom) => STerm atom -> LMGen sen atom [Term atom]
yudSolve term = do
  terms <- getTerms
  results <- for terms (yudSolve' term)
  return $ traceShowId $ concat results

-- THere are like fifteen things wrong with this
yudSolve' :: (Sentence sen Term, Atomic atom) => STerm atom -> STerm atom -> LMGen sen atom [Term atom]
yudSolve' term other = do
  let (term', frees) = intros term
  let (other', frees') = intros other
  let binds = traceShowId $ (term') >?> (other')
  if allowed frees binds then [unintros ((binds >@> term'), frees)] else []

solve :: forall sen atom. (Sentence sen Term, Atomic atom) => STerm atom -> LMGen sen atom (LogicResult ())
solve goal = do
  terms <- getTerms
  curDepth <- getDepth
  let _ = traceShowId terms
  let _ = traceShowId curDepth
  val <- mapM (solveStep :: STerm atom -> LMGen sen atom [Term atom]) terms
  addTerms $ fmap fmap fmap Sift.Base.Simple $ concat val
  nterms <- getTerms
  anySolve <- mapM (`doesSolve` goal) nterms
  if any id anySolve
    then return Solved
    else case curDepth of
      Nothing -> solve goal
      Just curDepthJ -> do
        setDepth $ Just (curDepthJ - 1)
        if curDepthJ > 0
          then
            solve goal
          else
            return Stopped

solveStep :: (Sentence sen Term) => STerm atom -> LMGen sen atom [Term atom]
solveStep interm = do
  gens <- yudGen interm
  reds <- yudRed interm
  solves <- yudSolve interm
  return $ gens <> reds <> solves
