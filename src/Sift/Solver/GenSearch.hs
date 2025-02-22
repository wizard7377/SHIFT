{-# LANGUAGE AllowAmbiguousTypes #-}
module Sift.Solver.GenSearch where

import Sift.Monad hiding (_depth)
import Sift.Monad()
import qualified Sift.Monad as Sift
import Rift
import Extra.Choice(Choice (AllOf,AnyOf,Simple,Trivial,EmptyNode))
import qualified Sift.Types (LogicEnv(..))
import Sift.Types (LogicEnv)
import Rift.Funcs (intros, introduce, unintros)
import Control.Monad.State
import Control.Monad (join)
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
getTerms :: LMGen tok [Term tok]
getTerms = do
  state <- get
  let sens = _sentences state
  let val = toTerm <$> sens
  return val

-- |Set the list of solved terms
setTerms :: [Term tok] -> LMGen tok () 
setTerms input = modify $ \s -> s {_sentences = fromTerm <$> input} 
-- |Add some terms to the list of solved terms
addTerms :: [Term tok] -> LMGen tok () 
addTerms add = do 
  terms <- getTerms
  setTerms (add ++ terms)

-- |Can the goal be solved for (by no steps) from the given
doesSolve :: Token a => 
  -- |Given
  Term a -> 
  -- |Goal
  Term a -> LMGen tok Bool 
doesSolve given goal = 
  let 
    givenT = introduce given 
    goalT = introduce goal
  in case goalT of
    _ | goodUnify $ goalT @? givenT -> return True

type PosSearch a = Choice (Term a)
type LMGen tok a = forall sen. Sentence sen tok => LM (SearchState sen tok) a
type SearchAction a = Token a => Term a -> LMGen a Bool

yudGen :: Term tok -> LMGen tok [Term tok]
yudGen val = return $ (let (term,vars) = intros val in 
  unintros (Rule term Yud) vars) : (Rule val Yud) : []

yudRed :: Term tok -> LMGen tok [Term tok]
yudRed val = _