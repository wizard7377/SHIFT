{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Tests.Mem where

import Control.Lens qualified as Lens
import Data.Foldable (Foldable (toList))
import Data.Text qualified as T
import Debug.Trace (traceShowId)
import Extra
import Extra.Parsers
import Rift qualified
import Short
import Sift qualified
import Sift.Core.Monad
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Tests.Common
