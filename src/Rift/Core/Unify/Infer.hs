module Rift.Core.Unify.Infer where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State
import Extra
import Rift.Core.Base
import Rift.Core.Unify.Base

allSplit :: [a] -> [([a], a, [a])]
allSplit [] = []
allSplit xs = [(take i xs, x, drop (i + 1) xs) | (i, x) <- zip [0 ..] xs]

-- | The central unifier
unifyInfer :: (Eq t, Term t) => t -> t -> MUnify t ()
unifyInfer t0' t1' = do
  env <- get
  let res = env ^. result
  -- Rewrite the top and bottom
  let t0 = mapUp res t0'
  let t1 = mapDown res t1'
  resolver t0 t1 <|> case (t0, t1) of
    (Cons a0 a1, Cons b0 b1) -> do
      r0 <- unifyInfer a0 b0
      r1 <- unifyInfer a1 b1
      pure $ r0 <> r1
    _ -> empty

-- | Resolve a top term to a variable
resolver :: (Eq t, Term t) => t -> t -> MUnify t ()
resolver top bottom = do
  env <- get
  -- Look for any possible top variable
  -- TODO add bottom
  res <-
    ( msum $
        ( \(binds, x, rest) -> do
            -- If top is equal to x, resolve it
            -- TODO if this should check for variables
            if (top == x)
              then
                ( do
                    freeUp .= binds ++ rest
                    pure ()
                )
              else
                ( do
                    -- Temporarily set the list of possible bindings to the var
                    freeUp .= binds
                    -- Attempt to unify the given value with `top`
                    res0 <- unifyInfer top x
                    -- Get the new state
                    state <- get
                    -- Set the new state to include those we ignored
                    freeUp .= (state ^. freeUp) ++ rest
                )
            return ()
        )
          <$> (allSplit $ env ^. freeUp) -- <|> _
      )
  return ()

resolveBottom :: t -> t -> MUnify t ()
resolveBottom = _
