{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
 - Generate the "conclusion"
 -
 - Shift all variables up, make all bindings simple
 -
-}
module Rift.Core.Unify.Conclude where

import Extra
import Rift.Core.Generate (Generator)
import Rift.Core.Instances (TermFull)
import Rift.Core.Unify.Base

data Concluding term
  = Binding term term
  | Free term
  deriving (Eq, Show, Ord, Data, Typeable, Generic)

type Conclusions term = [Concluding term]

conclude :: (TermFull tag term, Generator tag gen) => gen -> Choice (UnifyState term) -> ChoiceAccum gen (Conclusions term)
conclude = _

conclude' :: (TermFull tag term, Generator tag gen) => gen -> (UnifyState term) -> ChoiceAccum gen (Conclusions term)
conclude' gen = do
  _
