{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
type VTerm atom = Term (VAtom atom)

toVTerm :: (Functor Term) => Term atom -> VTerm atom
toVTerm term = Simple <$> term
fromVTerm :: (Functor Term) => (Traversable Term) => VTerm atom -> Maybe (Term atom)
fromVTerm term =
  mapM
    ( \case
        Simple a -> Just a
        Chet _ -> Nothing
    )
    term
