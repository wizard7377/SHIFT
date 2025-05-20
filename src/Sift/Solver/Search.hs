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
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Sift.Solver.Search (genSearch, genSearch', mem, mem') where

-- TODO apart from generally making this faster, also probs should do something for parital init

import Control.Lens (Lens', lens, makeLenses, over, set, view, (%=), (&), (+=), (.=), (.~), (^.), _1, _2)
import Control.Monad (join)
import Control.Monad.Identity (Identity (..))

-- import Extra.Choice (Choice (AllOf, AnyOf, EmptyNode, Simple, Trivial))

import Control.Monad.RWS (MonadReader (..))
import Control.Monad.Reader
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
import Extra.Monad (recurseM)
import Extra.Tuple
import Rift qualified
import Sift.Base qualified as Base
import Sift.Monad ()
import Sift.Monad hiding (_depth)
import Sift.Monad qualified as Sift
import Sift.Solver.Types

-- import Sift.Types.Unify

-- TODO temporary

unfoldStartGen :: forall t w term. (Rift.Term term, Rift.TermLike term, Monoid w, Rift.TermOf t ~ term, Rift.Theory t) => [STerm term] -> LM t w (SearchState term) [STerm term]
unfoldStartGen starts =
  do
    env <- ask
    let unfoldStart = (env ^. Rift.unfoldStart)
    let theory = env ^. Rift.theory
    let sentences = (\(Rift.Sentence t p) -> STerm t [] $ Rift.Given p) <$> Rift.getSentences theory
    res <-
      ( ( ( \(x :: [STerm term]) (y :: [STerm term]) -> do
              lr <- sequence $ mem <$> y <*> x
              rr <- sequence $ mem <$> x <*> y
              let res = mconcat lr <> mconcat rr
              pure res
          ) ::
            ([STerm term] -> [STerm term] -> LM t w (SearchState term) [STerm term])
        )
          sentences
        )
        starts

    pure res
genSearch ::
  (Rift.Term term, Rift.TermLike term, Monoid w, Rift.TermOf t ~ term, Rift.Theory t) =>
  term ->
  LM t w (SearchState term) (Rift.LogicResult term)
genSearch term = do
  env <- ask
  let sentences = (\(Rift.Sentence t p) -> STerm t [] $ Rift.Given p) <$> Rift.getSentences (env ^. Rift.theory)
  unfold <- asks (^. Rift.unfoldStart)
  nsentences <- (recurseM unfold unfoldStartGen sentences)
  genSearch' term
genSearch' ::
  (Rift.Term term, Rift.TermLike term, Monoid w) =>
  term ->
  LM t w (SearchState term) (Rift.LogicResult term)
genSearch' goal = do
  env <- ask
  state <- get
  let maxDepth = env ^. Rift.depth
  depths <- gets _depth
  provens <- gets _proven
  scratches <- gets _scratch
  results' <- sequence ((mem <$> provens <*> scratches) <> (mem <$> scratches <*> provens))
  let results = nub $ concat results'
  depth .= depths + 1
  proven .= nub (provens <> scratches)
  scratch .= nub (results <> scratches)
  ( if (resolved (results <> provens <> scratches) goal)
      then do ("Solved" ?> return (Rift.Solved goal))
      else (if depths < maxDepth then (do "Go again" ?> genSearch' goal) else "Stopped" ?> return Rift.Stopped)
    )

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
  let mr = Rift.mem' upFrom upTo (var : freeUp) down freeDown
   in pure $ (\(Rift.FTerm t v) -> STerm t v (Rift.Mem proofA proofB)) <$> mr
mem' _ _ = pure []
{-# INLINE mem' #-}
