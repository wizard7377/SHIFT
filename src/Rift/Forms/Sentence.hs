{-# LANGUAGE TypeFamilies #-}

module Rift.Forms.Sentence where

import Data.Data
import GHC.Generics
import Rift.Core qualified as Rift

-- | The type of sentneces, that is, a tuple of form @(t,Int)@
data Sentence t = Sentence t Int
  deriving (Show, Eq, Generic, Data)

-- | A theory, that is, something that has a list of sentences
class Theory t where
  -- | The inner terms
  type TermOf t

  -- | The value of the sentences
  getSentences :: t -> [Sentence (TermOf t)]

instance Theory [t] where
  type TermOf [t] = t
  getSentences t = uncurry Sentence <$> zip t [0 ..]
