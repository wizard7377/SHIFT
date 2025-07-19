{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Sift.Core.Types
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Sift.Core.Types where

import Data.Text qualified as T
import Extra
import Rift.Forms qualified as Rift
import Short

data OpTypes
  = AlphaConvert
  | MemReduce
  | ZetaReduce
  | DeltaReduce
  | FixReduce
  | UnifyConvert
  | NullConvert
  deriving (Eq, Ord, Data, Typeable, Generic, Show)

data Hypothesis e where
  -- | ∀x
  Bound :: (TO e) -> Hypothesis e
  -- | {α := β}
  Subst :: (TO e) -> (TO e) -> Hypothesis e
  -- | α |> ?
  Reduce :: TO e -> Hypothesis e
  -- | α ~ β
  Equal :: (TO e) -> (TO e) -> Hypothesis e
  -- | T
  Trivial :: Hypothesis e
  -- | α ==> β
  Given :: Hypothesis e -> Hypothesis e -> Hypothesis e
  -- | [ Α ... ]
  AllOf :: [Hypothesis e] -> Hypothesis e

data OpEnv e = OpEnv
  {
  }
  deriving (Typeable)

data Global e = Global
  { _logicEnv :: Rift.LogicEnv
  , _opTheory :: e
  , _generator :: Idx
  }

data VarState t = Unbound | Value t
  deriving (Eq, Show, Functor, Foldable, Traversable)
data Local e = Local
  { _goals :: [Hypothesis e]
  , _hypos :: [Hypothesis e]
  -- ^ The list of hypotheses, that is, in sequents, `H |- ...`
  , _localDepth :: Int
  , _history :: [(OpTypes, TO e)]
  }

instance Default (Local e) where
  def = Local [] [] 0 []

makeLenses ''OpEnv
makeLenses ''Local
makeLenses ''Global
makePrisms ''Hypothesis

instance (Show (TO e)) => Show (Hypothesis e) where
  show (Bound t) = "∀" <> show t
  show (Trivial) = "⊤"
  show (Subst t1 t2) = show t1 <> " -> " <> show t2
  show (Equal t1 t2) = show t1 <> " ~ " <> show t2

instance (Show e) => Show (Global e) where
  show (Global logicEnv opTheory 0) =
    "<" <> show logicEnv <> ", " <> show opTheory <> ">"

instance (Show (TO e), Show e) => Show (Local e) where
  show (Local goals hypos localDepth history) =
    unlines
      [ "Depth is: " <> show localDepth <> " and history is: " <> show history
      , unlines (show <$> hypos)
      , "|-"
      , unlines (show <$> goals)
      ]
