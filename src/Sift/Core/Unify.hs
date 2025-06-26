{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
 -
 - The unification module
 -
-}
module Sift.Core.Unify (unify, prep, unify', UnifyState (..), unifyGraph, freeLeft, freeRight, freeBoth) where

import Control.Applicative (Alternative (..))
import Control.Lens (re)
import Control.Lens qualified as Lens
import Control.Lens.Operators ((?~), (^..))
import Control.Monad (join)
import Data.Foldable (Foldable (..), asum)
import Data.List (delete)
import Data.List.Extra (notNull)
import Extra
import Extra.Choice.Instances ()
import Extra.Map.Direct
import Rift qualified

data UnifyState t = UnifyState
  { _freeLeft :: [t]
  , _freeRight :: [t]
  , _freeBoth :: [t]
  , _unifyGraph :: TMap Direction t t
  }
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

makeLenses ''UnifyState
prep :: (Rift.FTerm t, Rift.KTerm (Rift.Inner t), Rift.KTerm t) => [Rift.Inner t] -> t -> t -> (Rift.Inner t, Rift.Inner t, UnifyState (Rift.Inner t))
prep freeBoth t1 t2 =
  let
    freeLeft = t1 ^. Rift.frees
    freeRight = t2 ^. Rift.frees
   in
    (t1 ^. Rift.fterm, t2 ^. Rift.fterm, UnifyState freeLeft freeRight freeBoth mempty)

unify :: (Rift.FTerm t, Rift.KTerm (Rift.Inner t), Rift.KTerm t, Eq (Rift.Inner t), Show (Rift.Inner t)) => [Rift.Inner t] -> t -> t -> Choice (UnifyState (Rift.Inner t))
unify freeBoth t0' t1' =
  let (t0, t1, state) = (prep freeBoth t0' t1')
      res = unifyStep t0 t1 state
   in res
unify' :: (Rift.FTerm t, Rift.KTerm (Rift.Inner t), Rift.KTerm t, Eq (Rift.Inner t), Show (Rift.Inner t)) => t -> t -> Choice (UnifyState (Rift.Inner t))
unify' = unify []
unifyStep :: forall t. (Rift.KTerm t, Eq t, Show t) => t -> t -> UnifyState (t) -> Choice (UnifyState (t))
unifyStep t1 t2 state =
  -- TODO: check if free
  ( if (not $ null boundT1) || (not $ null boundT2)
      then
        let (f :: Choice t -> Choice t -> Choice (UnifyState t)) = (\x -> \y -> (join $ (unifyStep <$> (x) <*> (y)) <*> pure state))
         in case (not $ null boundT1, not $ null boundT2) of
              (True, True) -> f boundT1 boundT2
              (True, False) -> f boundT1 (pure t2)
              (False, True) -> f (pure t1) boundT2
              _ -> empty
      else empty
  )
    <|> case (t1, t2) of
      (Rift.Atom', Rift.Atom') ->
        if ((freeLeftN == 0) && (freeRightN == 0) && (freeBothLN == 0) && (freeBothRN == 0) && (t1 == t2)) then (pure state) else (bindLeft t1 t2 state <|> bindRight t1 t2 state <|> bindLeftBoth t1 t2 state <|> bindRightBoth t1 t2 state)
      (Rift.Atom', _) -> bindLeft t1 t2 state <|> bindLeftBoth t1 t2 state
      (_, Rift.Atom') -> bindRight t1 t2 state <|> bindRightBoth t1 t2 state
      (Rift.Kaf ta1 tb1, Rift.Kaf ta2 tb2) -> do
        state' <- unifyStep ta1 ta2 state
        state2 <- unifyStep tb1 tb2 state'
        pure state2
      _ -> empty
 where
  boundT1 = "Value1" <?> findLeft t1 ("State" <?> state)
  boundT2 = "Value2" <?> findRight t2 state
  freeLeftN = length (filter (== t1) (state ^. freeLeft))
  freeRightN = length (filter (== t2) (state ^. freeRight))
  freeBothLN = length (filter (== t1) (state ^. freeBoth))
  freeBothRN = length (filter (== t2) (state ^. freeBoth))

bindLeft :: (Eq t) => t -> t -> UnifyState t -> Choice (UnifyState t)
bindLeft from to state =
  let
    graph = state ^. unifyGraph
    isFree = from `elem` (state ^. freeLeft)
    graph1 = graph <> ((pure (from ->> to)) ^. seeImgMap)
    frees = delete from (state ^. freeLeft)
    state1 = state & freeLeft .~ (frees)
    state2 = state1 & unifyGraph .~ graph1
   in
    if isFree then pure $ state2 else empty

bindLeftBoth :: (Eq t) => t -> t -> UnifyState t -> Choice (UnifyState t)
bindLeftBoth from to state =
  let
    graph = state ^. unifyGraph
    isFree = from `elem` (state ^. freeBoth)
    graph1 = graph <> ((pure (from ->> to)) ^. seeImgMap)
    graph2 = graph1 <> ((pure (to <<- from)) ^. seeImgMap)
    frees = delete from (state ^. freeBoth)
    state1 = state & freeBoth .~ (frees)
    state2 = state1 & unifyGraph .~ graph2
   in
    if isFree then pure $ state2 else empty

bindRightBoth :: (Eq t) => t -> t -> UnifyState t -> Choice (UnifyState t)
bindRightBoth from to state =
  let
    graph = state ^. unifyGraph
    isFree = to `elem` (state ^. freeBoth)
    graph1 = graph <> ((pure (from <<- to)) ^. seeImgMap)
    graph2 = graph1 <> ((pure (to ->> from)) ^. seeImgMap)
    frees = delete to (state ^. freeBoth)
    state1 = state & freeBoth .~ (frees)
    state2 = state1 & unifyGraph .~ graph2
   in
    if isFree then pure $ state2 else empty
bindRight :: (Eq t) => t -> t -> UnifyState t -> Choice (UnifyState t)
bindRight from to state =
  let
    graph = state ^. unifyGraph
    isFree = to `elem` (state ^. freeRight)
    graph1 = graph <> ((pure (from <<- to)) ^. seeImgMap)
    frees = delete to (state ^. freeRight)
    state1 = state & freeRight .~ (frees)
    state2 = state1 & unifyGraph .~ graph1
   in
    if isFree then pure $ state2 else empty
findLeft :: (Eq t) => t -> UnifyState t -> Choice t
findLeft term state =
  let
    graph = state ^. unifyGraph
   in
    fromList $ filter (/= term) $ toList $ lmaprec graph term
findRight :: (Eq t) => t -> UnifyState t -> Choice t
findRight term state =
  let
    graph = state ^. unifyGraph
   in
    fromList $ filter (/= term) $ toList $ rmaprec graph term
