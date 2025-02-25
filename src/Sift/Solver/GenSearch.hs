{-# LANGUAGE AllowAmbiguousTypes, DeepSubsumption #-}
module Sift.Solver.GenSearch where

import Sift.Monad hiding (_depth)
import Sift.Monad()
import qualified Sift.Monad as Sift
import Rift
import Extra.Choice(Choice (AllOf,AnyOf,Simple,Trivial,EmptyNode))
import qualified Sift.Types (LogicEnv(..))
import Sift.Types (LogicEnv)
import Rift.Funcs (intros, introduce, unintros, (@>), shrink, replace, replace')
import Control.Monad.State
import Control.Monad (join)
import Data.Maybe (isJust, maybeToList)
import Data.Traversable
import Extra.List (forEach)
import Data.Foldable (Foldable(..))
import Data.List (singleton)
import Sift (LogicResult (..))
import Control.Monad.Identity (Identity)
import Debug.Trace (traceShow, traceShowId)
-- |The state of the Search monad with sentence `sen` on token `tok`
data SearchState sen tok = SearchState {
    -- |All proven statements
    _sentences :: [sen tok],
    -- |Current depth to end 
    _depth :: Maybe Int,
    -- |List of infered vars 
    _vars :: Int
}



instance EnterState SearchState where
  enterState :: LogicEnv -> [sen tok] -> SearchState sen tok
  enterState env sens = SearchState {
    _sentences = sens,
    _depth = Sift.Types._depth env,
    _vars = 0
  }

-- |Get the list of solved terms
getTerms :: forall sen tok. (Sentence sen tok, Token tok) => LMGen sen tok [Term tok]
getTerms = do
  state <- get
  let sens = _sentences state
  let val = toTerm <$> sens
  return val

-- |Set the list of solved terms
setTerms :: forall sen tok. (Sentence sen tok, Token tok) => [Term tok] -> LMGen sen tok ()
setTerms input = modify $ \s -> s {_sentences = fromTerm <$> input}
-- |Add some terms to the list of solved terms
addTerms :: forall sen tok. (Sentence sen tok, Token tok) => [Term tok] -> LMGen sen tok ()
addTerms add = do
  terms <- getTerms
  setTerms (add ++ terms)

getDepth :: forall sen tok. (Sentence sen tok, Token tok) => LMGen sen tok (Maybe Int)
getDepth = do
  state <- get
  let dep = _depth state
  return dep
setDepth :: forall sen tok. (Sentence sen tok, Token tok) => Maybe Int -> LMGen sen tok ()
setDepth dep = modify $ \s -> s {_depth = dep}
-- |Can the goal be solved for (by no steps) from the given
doesSolve :: (Sentence sen tok, Token tok) =>
  -- |Given
  Term a ->
  -- |Goal
  Term a -> LMGen sen tok Bool
doesSolve given goal =
  let
    givenT = introduce given
    goalT = introduce goal
  in case goalT of
    _ | isJust $ givenT @> goalT -> return True

type PosSearch a = Choice (Term a)
type LMGen sen tok a = (Sentence sen tok, Token tok) => LM (SearchState sen tok) a
--type SearchAction a = Token a => Term a -> LMGen a Bool

yudGen :: Sentence sen tok => Term tok -> LMGen sen tok [Term tok]
yudGen val = return $ (let (term,vars) = intros val in
  unintros (Rule term Yud) vars) : (Rule val Yud) : []

yudRed :: Sentence sen tok => Term tok -> LMGen sen tok [Term tok]
yudRed val =
  return $ let (term,vars) = intros val in
    case term of
      (Rule term1 Yud) -> [unintros term1 vars]

yudSolve :: (Sentence sen tok, Token tok) => Term tok -> LMGen sen tok [Term tok]
yudSolve apply | (Rule to from,vars) <- intros apply = do {
  terms <- getTerms ;
  res <- mapM (yudSolve' apply) terms  ;
  return $ concat res
} -- TODO something with vars?
yudSolve _ = return []
yudSolve' :: (Sentence sen tok, Token tok) => Term tok -> Term tok -> LMGen sen tok [Term tok]
yudSolve' apply iter | (Rule to from,vars) <- intros apply =
    return $ case shrink iter from of {
        Just (_,reps) -> singleton $ forEach replace' reps to ;
        _ -> []
    }
yudSolve' _ _ = return []

solve :: forall sen tok. (Sentence sen tok, Token tok) => Term tok -> LMGen sen tok (LogicResult ())
solve goal = do
  terms <- getTerms
  curDepth <- getDepth
  let _ = traceShowId terms 
  let _ = traceShowId curDepth
  val <- mapM (solveStep :: Term tok -> LMGen sen tok [Term tok])  terms
  addTerms $ concat val
  nterms <- getTerms
  anySolve <- mapM (`doesSolve` goal) nterms
  if any id anySolve then return Solved else case curDepth of {
    Nothing -> solve goal ;
    Just curDepthJ -> do {
      setDepth $ Just (curDepthJ - 1) ;
      if curDepthJ > 0 then
        solve goal
      else
        return Stopped
    }
  }

solveStep :: Sentence sen tok => Term tok -> LMGen sen tok [Term tok]

solveStep interm = do {
  gens <- yudGen interm ;
  reds <- yudRed interm ;
  solves <- yudSolve interm ;
  return $ gens <> reds <> solves
}