{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Sift.Core.Unify.Prep where

import Control.Applicative (asum, (<|>))
import Data.Graph.Inductive (Gr, Graph (..))
import Data.Graph.Inductive qualified as Graph
import Extra
import Extra.Graphic
import Rift qualified

data Portion t = TopSide t | MiddleSide t | BottomSide t
  deriving (Show, Eq, Ord)

toGraph' ::
  (Eq term, Rift.Term term, Ord term) =>
  -- | Terms free left
  [term] ->
  [term] ->
  HMap term ->
  Choice (Graphic Gr (Portion term) ())
toGraph' = toGraph
toGraph ::
  forall gr term.
  (Graph.Graph gr, Graph.DynGraph gr, Ord term) =>
  (Eq term, Rift.Term term) =>
  -- | Terms free left
  [term] ->
  [term] ->
  HMap term ->
  Choice (Graphic gr (Portion term) ())
toGraph vUp vDown hmap =
  let
    maps = fromMap hmap
    map0 =
      maps
        <&> ( \(f, t) ->
                let
                  f' = if f `elem` vUp then TopSide f else MiddleSide f
                  t' = if t `elem` vDown then BottomSide t else MiddleSide t
                 in
                  (f', t')
            )
    map1 ::
      Choice [(Portion term, Portion term)] =
        mconcat ((\(x, y) -> pure [(x, y)] <|> pure [(y, x)]) <$> map0)
    graphic = (newGraph . toMap <$> map1)
   in
    graphic
