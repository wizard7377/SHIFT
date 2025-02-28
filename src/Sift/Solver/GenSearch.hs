{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeepSubsumption #-}

module Sift.Solver.GenSearch where

import Control.Monad (join)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.Foldable (Foldable (..))
import Data.List (singleton)
import Data.Maybe (isJust, maybeToList)
import Data.Traversable
import Debug.Trace (traceShow, traceShowId)
import Extra.Choice (Choice (AllOf, AnyOf, EmptyNode, Simple, Trivial))
import Extra.List (forEach)
import Rift
import Rift (Atomic)
import Sift (LogicResult (..))
import Sift.Base (LogicEnv)
import Sift.Base qualified (LogicEnv (..))
import Sift.Monad ()
import Sift.Monad hiding (_depth)
import Sift.Monad qualified as Sift

-- | The state of the Search monad with sentence `sen` on token `atom`
data SearchState sen atom = SearchState
  { _sentences :: [sen atom]
  -- ^ All proven statements
  , _depth :: Maybe Int
  -- ^ Current depth to end
  , _vars :: Int
  -- ^ List of infered vars
  }

instance EnterState SearchState where
  enterState :: LogicEnv -> [sen atom] -> SearchState sen atom
  enterState env sens =
    SearchState
      { _sentences = sens
      , _depth = Sift.Types._depth env
      , _vars = 0
      }

-- | Get the list of solved terms
getTerms :: forall sen atom. (Sentence sen, Atomic atom) => LMGen sen atom [Term atom]
getTerms = do
  state <- get
  let sens = _sentences state
  let val = toTerm <$> sens
  return val

-- | Set the list of solved terms
setTerms :: forall sen atom. (Sentence sen, Atomic atom) => [Term atom] -> LMGen sen atom ()
setTerms input = modify $ \s -> s{_sentences = fromTerm <$> input}

-- | Add some terms to the list of solved terms
addTerms :: forall sen atom. (Sentence sen, Atomic atom) => [Term atom] -> LMGen sen atom ()
addTerms add = do
  terms <- getTerms
  setTerms (add ++ terms)

getDepth :: forall sen atom. (Sentence sen, Atomic atom) => LMGen sen atom (Maybe Int)
getDepth = do
  state <- get
  let dep = _depth state
  return dep
setDepth :: forall sen atom. (Sentence sen, Atomic atom) => Maybe Int -> LMGen sen atom ()
setDepth dep = modify $ \s -> s{_depth = dep}

-- | Can the goal be solved for (by no steps) from the given
doesSolve ::
  (Sentence sen, Atomic atom) =>
  -- | Given
  Term a ->
  -- | Goal
  Term a ->
  LMGen sen atom Bool
doesSolve given goal =
  let
    givenT = introduce given
    goalT = introduce goal
   in
    case goalT of
      _ | isJust $ givenT @> goalT -> return True

type PosSearch a = Choice (Term a)
type LMGen sen atom a = (Sentence sen, Atomic atom) => LM (SearchState sen atom) a

-- type SearchAction a = Atomic a => Term a -> LMGen a Bool

yudGen :: (Sentence sen) => Term atom -> LMGen sen atom [Term atom]
yudGen val =
  return $
    ( let (term, vars) = intros val
       in unintros (Rule term Yud) vars
    )
      : (Rule val Yud)
      : []

yudRed :: (Sentence sen) => Term atom -> LMGen sen atom [Term atom]
yudRed val =
  return $
    let (term, vars) = intros val
     in case term of
          (Rule term1 Yud) -> [unintros term1 vars]

yudSolve :: (Sentence sen, Atomic atom) => Term atom -> LMGen sen atom [Term atom]
yudSolve apply | (Rule to from, vars) <- intros apply = do
  terms <- getTerms
  res <- mapM (yudSolve' apply) terms
  return $ concat res -- TODO something with vars?
yudSolve _ = return []
yudSolve' :: (Sentence sen, Atomic atom) => Term atom -> Term atom -> LMGen sen atom [Term atom]
yudSolve' apply iter | (Rule to from, vars) <- intros apply =
  return $ case shrink iter from of
    Just (_, reps) -> singleton $ forEach replace' reps to
    _ -> []
yudSolve' _ _ = return []

solve :: forall sen atom. (Sentence sen, Atomic atom) => Term atom -> LMGen sen atom (LogicResult ())
solve goal = do
  terms <- getTerms
  curDepth <- getDepth
  let _ = traceShowId terms
  let _ = traceShowId curDepth
  val <- mapM (solveStep :: Term atom -> LMGen sen atom [Term atom]) terms
  addTerms $ concat val
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

solveStep :: (Sentence sen) => Term atom -> LMGen sen atom [Term atom]
solveStep interm = do
  gens <- yudGen interm
  reds <- yudRed interm
  solves <- yudSolve interm
  return $ gens <> reds <> solves
