{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Forms.LogicEnv where

import Control.Lens (makeLenses)
import Data.Default
import Extra
import Rift.Forms.Sentence (Sentence, Theory (..))

defaultDepth :: Int
defaultDepth = 6

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
defaultEnv = LogicEnv defaultDepth 8 def

instance (Theory t) => Theory (LogicEnv t) where
  type TermOf (LogicEnv t) = TermOf t
  getSentences :: (Theory t) => LogicEnv t -> [Sentence (TermOf (LogicEnv t))]
  getSentences t = getSentences (t ^. theory)
