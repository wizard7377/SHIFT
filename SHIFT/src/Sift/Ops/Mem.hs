{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift.Ops.Mem (memReduce) where

import Control.Applicative (Alternative (..))
import Control.Lens (transform, (<>=))
import Control.Monad.Morph
import Control.Monad.RWS qualified as M
import Control.Monad.Trans
import Data.Choice
import Data.Foldable (toList)
import Extra
import Rift qualified
import Sift.Core.Types
import Sift.Core.Unify
import Sift.Ops.Common

memReduce :: Reduce e
memReduce (Rift.Kaf (Rift.Lamed v f a b) i) =
  pure (Rift.Kaf f i)
    <|> ( do
            (hypos <>= (pure $ Bound v))
            (goals <>= (pure $ Equal a i))
            pure b
        )
memReduce _ = empty
