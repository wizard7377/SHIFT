{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Forms.Search where

import Control.Lens (makeLenses)
import Data.Default
import Extra
import Rift.Forms.Theory (Sentence, Theory (..))

defaultDepth :: Int
defaultDepth = 2
defaultUnfold :: Int
defaultUnfold = 4

{- | The type of the logic env
 Note that it is parameterized over the type of the _theory_, not the type of the terms or sentences
 As such, it should almost always be given the context of `Theory`
-}
data LogicEnv t = LogicEnv
  { _depth :: Int
  -- ^ The maxiumum depth
  , _unfoldStart :: Int
  , _theory :: t
  -- ^ The given statements assumed to be true
  }
  deriving (Generic)

makeLenses ''LogicEnv
defaultEnv :: (Default s) => LogicEnv s
defaultEnv = LogicEnv defaultDepth defaultUnfold def

instance (Theory t) => Theory (LogicEnv t) where
  type TermOf (LogicEnv t) = TermOf t
  getSentences :: (Theory t) => LogicEnv t -> [Sentence (TermOf (LogicEnv t))]
  getSentences t = getSentences (t ^. theory)

-- | The result of a logic solve
data LogicResult p
  = -- | Solved
    Solved p
  | -- | Proven unsolvable (only possible in certain solvers)
    Unsolved
  | -- | Stopped, most likely due to overrunning @_depth@
    Stopped
  deriving (Show, Eq)

instance Semigroup (LogicResult p) where
  Solved p <> x = x
  Unsolved <> _ = Unsolved
  Stopped <> _ = Stopped
