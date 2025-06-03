{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sift.Solver.Types where

import Control.Lens (Lens', lens, makeLenses)
import Data.List (intercalate)
import Extra
import Rift qualified
import Sift.Monad

{- | The type of free terms, central to the algorithm ג and specifically the מ rule
 - This has a `Term'` part, `_term`, as well as a set of frees, `_frees`
 -
-}
data STerm term = STerm
  { _term :: term
  -- ^ The fundemental inner term
  , _frees :: [term]
  -- ^ The list of variables in the term
  , _proof :: Rift.Proof Int
  }
  deriving (Generic, Data)

instance (Rift.KTerm term) => Rift.FTerm (STerm term) where
  type Inner (STerm term) = term
  fterm = lens _term (\(STerm t fs i) t' -> STerm t' fs i)
  ffrees = lens _frees (\(STerm t fs i) fs' -> STerm t fs' i)
deriving instance (Eq a) => Eq (STerm a)
deriving instance (Ord a) => Ord (STerm a)

makeLenses ''STerm

showSTerm :: (Show a) => STerm a -> [Char]
showSTerm (STerm term frees i) = concatMap (\x -> "∀" ++ show x) frees ++ " . " ++ show term ++ " by " ++ show i

instance (Show term) => Show (STerm term) where
  show = showSTerm

addFrees :: STerm term -> [term] -> STerm term
addFrees (STerm t fs i) new = STerm t (fs <> new) i
replaceTerm :: STerm term -> term -> STerm term
replaceTerm (STerm _ fs i) new = STerm new fs i

reduceTerm :: (Eq term, Rift.KTerm term) => STerm term -> STerm term
reduceTerm (STerm t fs i) =
  let
    vs = filter (Rift.poccurs t) fs
   in
    STerm t vs (if fs == vs then i else Rift.Reduce i)

-- TODO
-- type LMGen term t w a = LM t w (SearchState term) w a

{- | A "solved term"
 That is, a term, along with it's proof
-}

-- | The state of the Search monad with sentence `sen` on token `term`
data SearchState term = SearchState
  { _proven :: [STerm term]
  , _scratch :: [STerm term]
  -- ^ All proven statements
  , _depth :: Int
  -- ^ Current depth to end
  , _vars :: Int
  }
  deriving (Generic, Data)

depth :: Lens' (SearchState term) Int
depth = lens _depth (\s d -> s{_depth = d})
scratch :: Lens' (SearchState term) [STerm term]
scratch = lens _scratch (\s d -> s{_scratch = d})
proven :: Lens' (SearchState term) [STerm term]
proven = lens _scratch (\s d -> s{_proven = d})
vars :: Lens' (SearchState term) Int
vars = lens _vars (\s d -> s{_vars = d})
fterm :: Lens' (STerm term) (Rift.FTerm' term)
fterm = lens (\(STerm t v i) -> (Rift.FTerm' t v)) (\(STerm t v i) (Rift.FTerm' t' v') -> (STerm t' v' i))
showState :: (Rift.KTerm term, Rift.TermLike term) => SearchState term -> String
showState state =
  let
    header = "Depth is: " ++ show (state ^. depth)
    provenv = "\n\nProven\n\n" ++ intercalate "\n" (show <$> (state ^. proven))
    scratchv = "\n\nScratch\n\n" ++ intercalate "\n" (show <$> (state ^. scratch))
   in
    header ++ provenv ++ scratchv ++ "\n"
instance (Rift.KTerm term, Rift.TermLike term) => Show (SearchState term) where
  show = showState
instance forall term. EnterState (SearchState term) where
  type TermOf (SearchState term) = term
  enterState ::
    (Rift.Theory t, Rift.TermOf t ~ TermOf (SearchState term)) =>
    Rift.LogicEnv t ->
    SearchState term
  enterState env =
    SearchState
      { _scratch = (\(Rift.Sentence t p) -> (STerm t [] $ Rift.Given p)) <$> Rift.getSentences theory
      , _proven = (\(Rift.Sentence t p) -> (STerm t [] $ Rift.Given p)) <$> Rift.getSentences theory
      , _depth = 0
      , _vars = 0
      }
   where
    theory = env ^. Rift.theory

showTerms :: (Rift.TermLike term) => [term] -> String
showTerms sens = concatMap (\s -> show s) sens
showSens :: (Rift.TermLike term) => SearchState term -> String
showSens = showTerms . _scratch
