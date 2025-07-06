{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Extra.Graphic where

import Control.Lens qualified as Lens
import Data.Graph.Inductive
import Data.List (singleton)
import Data.List.Extra (intercalate)
import Extra.Lens
import Extra.Map

graphToMap :: (Graph gr) => gr a b -> TMap b a a
graphToMap gr = _
