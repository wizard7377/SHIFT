{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Lift.Core.Dev.Parse where

import Data.Text qualified as T
import Lift.Core.Forms.Module
import Lift.Core.Monad
import Rift qualified
import Rift.Forms.Language
import Text.Megaparsec qualified as T

testLiftState :: _ -> LiftState t
testLiftState file =
  LiftState (Universe mempty) (pure $ T.pack "") file
testFMT :: FMT t m a -> m _
testFMT comp =
  runFMT comp defaultParseEnv $ testLiftState "TEST"

testParse comp = T.parseTest (testFMT comp)
testParseFile comp file = do
  input <- readFile file
  output <- T.parseTest (runFMT comp defaultParseEnv $ testLiftState "file") input
  pure output
