{-# LANGUAGE PartialTypeSignatures #-}

module Sift.Search.Reduce where

import Control.Applicative (Alternative (..))
import Control.Monad.Morph
import Control.Monad.RWS qualified as M
import Control.Monad.Reader
import Extra
import Data.Choice
import Rift qualified
import Sift.Core.Types
import Sift.Ops.Common
import Sift.Ops.Fix
import Sift.Ops.Mem
import Sift.Ops.Simple
import Sift.Ops.Zeta
import Sift.Search.Convert

convert :: Convert
convert = simpleConvert <|> alphaConvert <|> nullConvert
reduce :: Redux
reduce t = memReduce t <|> zetaReduce t <|> simpleReduce t <|> fixReduce t
unify :: Unify
unify = unifyConvert
convertRec :: Convert
convertRec = _
reduceRec :: Redux
reduceRec t = do
  t' <- reuduce t
  _
