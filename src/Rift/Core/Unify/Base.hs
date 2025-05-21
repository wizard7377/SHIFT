{-# LANGUAGE TemplateHaskell #-}

module Rift.Core.Unify.Base where

import Control.Applicative
import Control.Lens (Lens', makeLenses)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Extra

-- | The state of a term, where it being bound always features the variable first
data TermState a = Free a | Bound a a
  deriving (Show, Eq, Data, Typeable, Generic, Ord)

data UnifyState term = UnifyState
  { _upState :: [TermState term]
  , _sharedState :: [TermState term]
  , _downState :: [TermState term]
  }
  deriving (Ord, Data, Typeable, Generic)

deriving instance (Show term) => Show (UnifyState term)
deriving instance (Eq term) => Eq (UnifyState term)
data UnifyResult term = UnifyResult
  { _upToDown :: HMap term
  , _downToUp :: HMap term
  }
  deriving (Show, Eq, Data, Typeable, Generic, Ord)

makeLenses ''UnifyState
makeLenses ''UnifyResult

mapDown :: (Eq term) => UnifyResult term -> term -> term
mapDown = mapDownUp' []
mapUp :: (Eq term) => UnifyResult term -> term -> term
mapUp = mapUpDown' []
mapUpDown' :: (Eq term) => [term] -> UnifyResult term -> term -> term
mapUpDown' used res from =
  if (from `elem` used)
    then from
    else case lookup from (res ^. upToDown) of
      Just to -> mapDownUp' (from : used) res to
      Nothing -> from
mapDownUp' :: (Eq term) => [term] -> UnifyResult term -> term -> term
mapDownUp' used res from =
  if (from `elem` used)
    then from
    else case lookup from (res ^. upToDown) of
      Just to -> mapUpDown' (from : used) res to
      Nothing -> from

instance Semigroup (UnifyResult t) where
  (UnifyResult a b) <> (UnifyResult c d) = UnifyResult (a <> c) (b <> d)
instance Monoid (UnifyResult t) where
  mempty = UnifyResult mempty mempty
instance (Eq t) => Semigroup (UnifyState t) where
  (UnifyState a b c) <> (UnifyState d e f) =
    UnifyState (a <> d) (b <> e) (c <> f)

newtype MUnify t r = MUnify {runUnify :: (StateT (UnifyState t) Choice) r}
instance Monad (MUnify t) where
  (MUnify m) >>= f = MUnify $ m >>= runUnify . f

instance Functor (MUnify t) where
  fmap f (MUnify m) = MUnify $ fmap f m
instance Applicative (MUnify t) where
  pure = MUnify . pure
  (MUnify m) <*> (MUnify n) = MUnify $ m <*> n

instance MonadState (UnifyState t) (MUnify t) where
  get = MUnify $ get
  put s = MUnify $ put $ s

instance (Eq t, Semigroup r) => Semigroup (MUnify t r) where
  (MUnify m) <> (MUnify n) = MUnify $ (<>) <$> m <*> n
instance (Eq t, Monoid r) => Monoid (MUnify t r) where
  mempty = MUnify $ return mempty

instance Alternative (MUnify t) where
  empty = MUnify $ empty
  (MUnify a) <|> (MUnify b) = MUnify $ a <|> b

instance MonadPlus (MUnify t)

runUnifyM :: MUnify t r -> UnifyState t -> [(r, UnifyState t)]
runUnifyM (MUnify m) = runChoice <$> runStateT m
