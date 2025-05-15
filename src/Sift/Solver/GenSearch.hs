{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeepSubsumption #-}
{-# HLINT ignore "Use mapMaybe" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- TODO IMPLEMENT lineararity
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Sift.Solver.GenSearch where

-- TODO apart from generally making this faster, also probs should do something for parital init

import Control.Lens (Lens', lens, makeLenses, over, set, view, (%=), (&), (+=), (.=), (.~), (^.), _1, _2)
import Control.Monad (join)
import Control.Monad.Identity (Identity (..))

-- import Extra.Choice (Choice (AllOf, AnyOf, EmptyNode, Simple, Trivial))

import Control.Monad.RWS (MonadReader (..))
import Control.Monad.State
import Data.Foldable (Foldable (..), asum)
import Data.List (intercalate, intersect, singleton, subsequences, (\\))
import Data.List.Extra (nub)
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Data.Traversable
import Debug.Trace (traceShow, traceShowId)
import Extra
import Extra.Basics
import Extra.Choice hiding (resolve)
import Extra.Choice qualified
import Extra.Error (Error)
import Extra.List (forEach, subParts)
import Extra.Map
import Extra.Tuple
import Rift qualified
import Sift.Base (LogicResult (..))
import Sift.Base qualified as Base
import Sift.Monad ()
import Sift.Monad hiding (_depth)
import Sift.Monad qualified as Sift

-- import Sift.Types.Unify

-- TODO temporary

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

depth :: Lens' (SearchState term) Int
depth = lens _depth (\s d -> s{_depth = d})
scratch :: Lens' (SearchState term) [STerm term]
scratch = lens _scratch (\s d -> s{_scratch = d})
proven :: Lens' (SearchState term) [STerm term]
proven = lens _scratch (\s d -> s{_proven = d})
vars :: Lens' (SearchState term) Int
vars = lens _vars (\s d -> s{_vars = d})

showState :: (Rift.Term term, Rift.TermLike term) => SearchState term -> String
showState state =
  let
    header = "Depth is: " ++ show (state ^. depth)
    provenv = "\n\nProven\n\n" ++ intercalate "\n" (show <$> (state ^. proven))
    scratchv = "\n\nScratch\n\n" ++ intercalate "\n" (show <$> (state ^. scratch))
   in
    header ++ provenv ++ scratchv ++ "\n"
instance (Rift.Term term, Rift.TermLike term) => Show (SearchState term) where
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
genSearch ::
  (Rift.Term term, Rift.TermLike term, Monoid w) =>
  term ->
  LM t w (SearchState term) LogicResult
genSearch = genSearch'
genSearch' ::
  (Rift.Term term, Rift.TermLike term, Monoid w) =>
  term ->
  LM t w (SearchState term) LogicResult
genSearch' goal = do
  env <- ask
  state <- get
  let maxDepth = env ^. Rift.depth
  depths <- gets _depth
  provens <- gets _proven
  scratches <- gets _scratch
  results' <- sequence ((mem <$> provens <*> scratches) <> (mem <$> scratches <*> provens))
  let results = nub $ concat $ results'
  depth .= depths + 1
  proven .= provens <> scratches
  scratch .= results <> scratches
  res <-
    ( if (resolved (results <> provens <> scratches) goal)
        then do ("Solved" ?> return Solved)
        else (if depths < maxDepth then (do "Go again" ?> genSearch' goal) else "Stopped" ?> return Stopped)
      )
  return res

resolved ::
  (Rift.Term term, Rift.TermLike term) =>
  [STerm term] ->
  term ->
  Bool
resolved vals goal =
  "Called"
    ?> any
      ( \(STerm t v i) ->
          let
            bindc = "Bindc" <?> Rift.generate goal t
            env = "env" <?> Rift.initEnv [] v
            unifyResultChoice = "Result" <?> concat $ Rift.unify <$> bindc <*> pure env
           in
            (not (null unifyResultChoice))
      )
      vals

-- | The basic mem rule
mem ::
  (Rift.Term term, Eq term, Ord term, Show term, Monoid w) =>
  -- | The transform, top value $?x A B$
  STerm term ->
  -- | The transformed, bottom value, $B$
  STerm term ->
  LM t w (SearchState term) [STerm term]
mem top bottom =
  (mem' top bottom) >>= \case
    [] -> return []
    vals -> do
      newvals <- concat <$> sequence ((mem <$> vals) <*> pure bottom)
      pure $ vals <> newvals
{-# INLINE mem #-}

mem' :: (Rift.Term term, Eq term, Ord term, Show term, Monoid w) => STerm term -> STerm term -> LM t w (SearchState term) [STerm term]
mem' top@(STerm (Rift.Lamed var upFrom upTo) freeUp proofA) bottom@(STerm down freeDown proofB) =
  let
    uni = Rift.generate upFrom down
    vUp = var : freeUp
    vDown = freeDown
    env = (pure Rift.initEnv) <*> pure (var : freeUp) <*> pure freeDown
    ur = concat $ Rift.unify <$> uni <*> env
    res =
      ( \ures ->
          let
            mapping = mapToF $ ures ^. Rift.lowering
            newvars = vUp \\ ures ^. Rift.upBinds
           in
            STerm (Rift.recurseSome mapping upTo) newvars (Rift.Mem proofA proofB)
      )
        <$> ur
   in
    pure res
mem' _ _ = pure []
{-# INLINE mem' #-}
