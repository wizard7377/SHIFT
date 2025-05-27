{-# LANGUAGE LambdaCase #-}

module Rift.Core.Unify.Infer where

import Control.Applicative
import Control.Lens (Each (..))
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Extra
import Rift.Core.Base
import Rift.Core.Unify.Base

allSplit :: [a] -> [([a], a, [a])]
allSplit [] = []
allSplit xs = [(take i xs, x, drop (i + 1) xs) | (i, x) <- zip [0 ..] xs]

-- | The central unifier
unifyInfer :: (TermLike t, Term t) => Unification t
unifyInfer t0 t1 state =
  "Result" <?> do
    -- Rewrite the top and bottom
    (simpler t0 t1 state) <|> case (t0, t1) of
      (Cons a0 a1, Cons b0 b1) -> do
        -- Unify the heads
        res0 <- unifyInfer a0 b0 state
        -- Unify the tails
        res1 <- unifyInfer a1 b1 res0
        -- Return the result
        pure res1
      (_, Cons _ _) -> resolveUp t0 t1 state
      (Cons _ _, _) -> resolveDown t0 t1 state
      (_, _) -> resolveUp t0 t1 state <|> resolveDown t0 t1 state

resolveUp :: Unification t
resolveUp v t state = if v `isFree` (state ^. upState) then pure $ state & upState %~ (setAt v (Bound v t)) else cabsurd
resolveDown t v state = if v `isFree` (state ^. downState) then pure $ state & downState %~ (setAtB v (Bound t v)) else cabsurd
simpler :: Unification t
simpler x y state =
  if (top == bottom) && (not $ top `isFree` (state ^. upState)) && (not $ bottom `isFree` (state ^. downState))
    then pure state
    else cabsurd
 where
  top = mapUp state x
  bottom = mapDown state y
