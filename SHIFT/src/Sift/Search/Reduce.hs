{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Sift.Search.Reduce where

import Control.Applicative (Alternative (..))
import Control.Lens as L
import Control.Monad qualified as M
import Control.Monad.Combinators (choice)
import Control.Monad.Combinators qualified as M
import Control.Monad.Morph
import Control.Monad.RWS qualified as M
import Control.Monad.Reader
import Data.Choice
import Data.Traversable (for)
import Extra
import Rift qualified
import Short
import Sift.Core.Types
import Sift.Ops.Common
import Sift.Ops.Fix
import Sift.Ops.Mem
import Sift.Ops.Simple
import Sift.Ops.Unify (unifyConvert)
import Sift.Ops.Zeta
import Sift.Search.State

convert :: forall e. (TOC e) => Convert e
reduce :: (TOC e) => Reduce e
convertRec :: (TOC e) => Convert e
reduceRec :: (TOC e) => Reduce e
convert x y =
  choice
    [ (addStep AlphaConvert (alphaConvert x) $ y)
    , (addStep UnifyConvert (unifyConvert x) $ y)
    ]

-- | Perform exactly one `מפζ` reduction
reduce x =
  "TRY reduce"
    ?> choice
      [ (addStep MemReduce memReduce $ x)
      , (addStep FixReduce fixReduce $ x)
      , (addStep ZetaReduce zetaReduce $ x)
      , (addStep DeltaReduce deltaReduce $ x)
      ]

reduceRec x = do
  ldepth <- L.use localDepth
  gdepth <- lift $ lift $ L.use (logicEnv . Rift.logicEnvDepth)
  localDepth += 1
  M.guard (ldepth < gdepth)
  r <- pure $ reduceRec x
  (r <|> pure x <|> reduce x)

convertRec x y = do
  (global :: Global e) <- lift $ lift M.get
  (local :: Local e) <- M.get
  let gdepth = global ^. logicEnv . Rift.logicEnvDepth
  let ldepth = local ^. localDepth
  let lgoals = local ^. goals
  localDepth += 1
  M.guard (ldepth < gdepth)
  if x == y
    then
      pure y
    else
      ( do
          x' <- pure x <|> reduce x
          y' <- pure y <|> reduce y
          (convert x' y') <|> (convertRec x' y')
      )
