{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift.Ops.Zeta (zetaReduce) where

import Control.Applicative (Alternative (..), (<|>))
import Data.Functor (($>))
import Extra
import Rift qualified
import Sift.Core.Types
import Sift.Ops.Common

-- | ζ reduce, that is, replace @|- עαβγ@ with @α -> β |- γ{α/β}@
zetaReduce :: Reduce e
zetaReduce start@(Rift.Ayin from to within) = pure start <|> ((request (Subst from to)) >> (resolve within))
zetaReduce _ = empty
