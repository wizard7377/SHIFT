{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift.Ops.Zeta (zetaReduce) where

import Control.Applicative (Alternative (..), (<|>))
import Extra
import Rift qualified
import Sift.Ops.Common

zetaReduce :: Redux
zetaReduce start@(Rift.Ayin from to within) = pure start <|> (pure $ transform (change from to) within)
zetaReduce _ = empty
