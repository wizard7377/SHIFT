{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
 - Generate the "conclusion"
 -
 - Shift all variables up, make all bindings simple
 -
-}
module Sift.Core.Unify.Conclude where

import Control.Lens qualified as Lens
import Data.Maybe (isJust, isNothing)
import Extra
import Rift.Core.Base (TermLike)
import Rift.Core.Generate (Generator, generateTerm)
import Rift.Core.Instances (TermFull)
import Rift.Core.Interface (UTerm (..))
import Sift.Core.Unify.Base hiding (Free)
import Sift.Core.Unify.Base qualified as Unify

data Concluding term
  = Binding term term
  | Free term
  deriving (Eq, Show, Ord, Data, Typeable, Generic)

type Conclusions term = [Concluding term]
data Renaming term = Renaming term term
toConcluding :: TermState term -> Concluding term
toConcluding (Unify.Free free) = (Free free)
toConcluding (Unify.Bound bound free) = (Binding bound free)
iscyclic :: (TermLike term) => UnifyState term -> [term] -> Bool
-- TODO
iscyclic state terms = any (isNothing) $ (mapUp' [] state) <$> terms
upNormal :: (TermLike term) => UnifyState term -> Choice (UnifyState term)
upNormal state =
  let
    state1 = state & upState . each . _Bound %~ (\(x, y) -> (x, mapDown state y))
   in
    pure state1

downResolve :: (UTerm gen term, TermLike term) => gen -> UnifyState term -> (UnifyState term)
downResolve gen state =
  let
    frees = getFree (state ^. downState)
    frees' = (\x -> (x, uniqueCreate x gen)) <$> frees
    state1 = foldr (\free state -> (uncurry replaceDown) free state) state frees'
   in
    -- TODO
    (state1)
conclude :: (TermFull tag term) => tag -> Choice (UnifyState term) -> Choice (UnifyState term)
conclude gen state = conclude' gen <| state

conclude' :: (TermFull tag term) => tag -> (UnifyState term) -> Choice (UnifyState term)
conclude' gen state =
  if iscyclic state (getBound (state ^. upState))
    then cabsurd
    else do
      let (state1) = downResolve gen state
      state2 <- upNormal state1
      pure (state2)
