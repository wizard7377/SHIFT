{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Sift.Types.Generate where

import Rift.Core.Base
import Rift.Core.Instances ()

type Idx = Int
newtype Generator = Generator Idx

heGen :: Generator -> (VAtom atom, Generator)
heGen (Generator n) = (Chet n, Generator (n + 1))

data VAtom atom where
  Simple :: atom -> VAtom atom
  Chet :: Idx -> VAtom atom
  deriving (Eq, Ord)

type VTerm atom = Term (VAtom atom)

instance (Show atom) => Show (VAtom atom) where
  show (Simple a) = show a
  show (Chet id) = show "_" ++ show id
toVTerm :: (Functor Term) => Term atom -> VTerm atom
toVTerm term = Simple <$> term
fromVTerm :: (Functor Term) => (Traversable (GenericTerm BaseCons)) => VTerm atom -> Maybe (Term atom)
fromVTerm term =
  mapM
    ( \case
        Simple a -> Just a
        Chet _ -> Nothing
    )
    term
