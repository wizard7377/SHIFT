{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Control.Applicative
import Control.Monad.Extra (ifM)
import Control.Monad.RWS (MonadReader (..))
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (Foldable (..), asum)
import Data.List (intercalate, intersect, singleton, subsequences, (\\))
import Data.List.Extra (nub)
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Data.Traversable
import Debug.Trace (traceShow, traceShowId)
import Extra hiding (flip)
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

{-# SCC genSearch #-}
{-# SCC genSearch' #-}
{-# SCC mem #-}
{-# SCC mem' #-}
unfoldStartGen :: forall t w term. (Rift.Term term, Rift.TermLike term, Monoid w, Rift.TermOf t ~ term, Rift.Theory t, Rift.FTermLike term, Rift.UTermLike term Int) => Choice (STerm term) -> LM t w (SearchState term) (Choice (STerm term))
unfoldStartGen = unfoldStartGen' 0
unfoldStartGen' :: forall t w term. (Rift.Term term, Rift.TermLike term, Monoid w, Rift.TermOf t ~ term, Rift.Theory t, Rift.FTermLike term, Rift.UTermLike term Int) => Int -> Choice (STerm term) -> LM t w (SearchState term) (Choice (STerm term))
unfoldStartGen' i starts =
  do
    env <- ask
    let unfoldStart = (env ^. Rift.unfoldStart)
    let theory = env ^. Rift.theory
    let sentences = (\(Rift.Sentence t p) -> STerm t [] $ Rift.Given p) <$> Rift.getSentences theory
    res <-
      ( ( \(x :: Choice (STerm _)) (y :: Choice (STerm _)) -> do
            lr <- sequence $ mem <$> y <*> x
            rr <- sequence $ mem <$> x <*> y
            let res = asum (lr <|> rr)
            pure res
        )
          $ mkChoice sentences
        )
        starts
    let res' = csimpl' res
    if (unfoldStart < i) then pure res' else (unfoldStartGen' (i + 1) res')
genSearch ::
  (Rift.TermLike term, Rift.Term term, Monoid w, Rift.Theory t, Rift.TermOf t ~ term, Rift.UTermLike term Int, Rift.FTermLike term) =>
  term ->
  LM t w (SearchState term) (Rift.LogicResult term)
genSearch term = do
  env <- ask
  let sentences = (\(Rift.Sentence t p) -> STerm t [] $ Rift.Given p) <$> Rift.getSentences (env ^. Rift.theory)
  unfold <- asks (^. Rift.unfoldStart)
  nsentences <- (recurseM unfold unfoldStartGen $ mkChoice sentences)
  genSearch' term
genSearch' ::
  (Rift.TermLike term, Rift.Term term, Monoid w, Rift.UTermLike term Int, Rift.FTermLike term) =>
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
  let results = runChoice $ csimpl' $ asum results'
  depth .= depths + 1
  proven .= nub (provens <|> scratches)
  scratch .= nub (results <|> scratches)
  isres <- (resolved (nub (results <|> provens <|> scratches)) goal)
  res <-
    if isres
      then pure $ Rift.Solved goal
      else (if depths < maxDepth then genSearch' goal else return Rift.Stopped)
  pure res

resolved ::
  (Rift.TermLike term, Rift.Term term, Monoid w) =>
  [STerm term] ->
  term ->
  LM t w (SearchState term) Bool
resolved vals goal =
  let
    ures = Rift.unify (Rift.FTerm goal []) <$> vals
    res = cexists <$> ures
   in
    pure $ or res

-- | The basic mem rule
mem ::
  -- \| The transform, top value $?x A B$
  (Rift.Term term, Rift.TermLike term, Monoid w, Rift.FTermLike term, Rift.UTermLike term Int) =>
  STerm term ->
  -- | The transformed, bottom value, $B$
  STerm term ->
  LM t w (SearchState term) (Choice (STerm term))
mem top bottom =
  "Mem"
    ?> ( \case
          Choice [] -> cabsurd
          vals -> vals
       )
    <$> mem' top bottom

-- {-# INLINE mem #-}

mem' :: (Rift.Term term, Eq term, Ord term, Show term, Monoid w, Rift.FTermLike term, Rift.UTermLike term Int) => STerm term -> STerm term -> LM t w (SearchState term) (Choice (STerm term))
mem' top@(STerm (Rift.Lamed var upFrom upTo) freeUp proofA) bottom@(STerm down freeDown proofB) = do
  state <- get
  let mr = Rift.memReduce (state ^. depth) (var : freeUp) upFrom upTo (down, freeDown)
  let res = (\(Rift.FTerm t v) -> STerm t v (Rift.Mem proofA proofB)) <$> mr
  "Not absurd" ?> (pure res)
mem' _ _ = do pure $ "Absurd" ?> cabsurd

-- {-# INLINE mem' #-}
