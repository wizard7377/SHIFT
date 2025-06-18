{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
 -
 - The unification module
 -
-}
module Sift.Core.Unify where

import Control.Applicative (Alternative (..))
import Control.Lens (re)
import Control.Lens qualified as Lens
import Control.Lens.Operators ((?~), (^..))
import Data.Foldable (Foldable (..), asum)
import Data.List (delete)
import Data.List.Extra (notNull)
import Extra
import Extra.Map.Direct
import Rift qualified

data UnifyState t = UnifyState
  { _freeLeft :: [t]
  , _freeRight :: [t]
  , _unifyGraph :: TMap Direction t t
  }
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

makeLenses ''UnifyState
prep :: (Rift.FTerm t, Rift.KTerm (Rift.Inner t), Rift.KTerm t) => t -> t -> (Rift.Inner t, Rift.Inner t, UnifyState (Rift.Inner t))
prep t1 t2 =
  let
    freeLeft = t1 ^. Rift.frees
    freeRight = t2 ^. Rift.frees
   in
    (t1 ^. Rift.fterm, t2 ^. Rift.fterm, UnifyState freeLeft freeRight mempty)

unify :: (Rift.FTerm t, Rift.KTerm (Rift.Inner t), Rift.KTerm t, Eq (Rift.Inner t), Show (Rift.Inner t)) => t -> t -> Choice (UnifyState (Rift.Inner t))
unify t0' t1' =
  let (t0, t1, state) = (prep t0' t1')
      res = unify' t0 t1 state
   in res
unify' :: (Rift.KTerm t, Eq t, Show t) => t -> t -> UnifyState t -> Choice (UnifyState (t))
unify' t1 t2 state = unifyStep t1 t2 state

unifyStep :: forall t. (Rift.KTerm t, Eq t, Show t) => t -> t -> UnifyState (t) -> Choice (UnifyState (t))
unifyStep t1 t2 state =
  -- TODO: check if free
  ( if (cexists boundT1) || (cexists boundT2)
      then
        let (f :: Choice t -> Choice t -> Choice (UnifyState t)) = \x -> \y -> (asum $ (unify' <$> (x) <*> (y)) <*> pure state)
         in case (cexists boundT1, cexists boundT2) of
              (True, True) -> f boundT1 boundT2
              (True, False) -> f boundT1 (pure t2)
              (False, True) -> f (pure t1) boundT2
              _ -> empty
      else empty
  )
    <|> case (t1, t2) of
      (Rift.Atom', Rift.Atom') -> if ((freeLeftN == 0) && (freeRightN == 0) && (t1 == t2)) then (pure state) else (bindLeft t1 t2 state <|> bindRight t1 t2 state)
      (Rift.Atom', _) -> bindLeft t1 t2 state
      (_, Rift.Atom') -> bindRight t1 t2 state
      (Rift.Kaf ta1 tb1, Rift.Kaf ta2 tb2) -> do
        state' <- unify' ta1 ta2 state
        state2 <- unify' tb1 tb2 state'
        pure state2
      _ -> empty
 where
  boundT1 = "Value1" <?> findLeft t1 ("State" <?> state)
  boundT2 = "Value2" <?> findRight t2 state
  freeLeftN = length (filter (== t1) (state ^. freeLeft))
  freeRightN = length (filter (== t2) (state ^. freeRight))

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
    fromFoldable $ filter (/= term) $ toList $ lmaprec graph term
findRight :: (Eq t) => t -> UnifyState t -> Choice t
findRight term state =
  let
    graph = state ^. unifyGraph
   in
    fromFoldable $ filter (/= term) $ toList $ rmaprec graph term
