{-# LANGUAGE TemplateHaskell #-}

module Sift.Core.Unify.Algorithm where

import Control.Lens.Operators ((^..))
import Data.Graph.Inductive
import Data.Graph.Inductive qualified as Graph
import Extra
import Extra.Graphic
import Sift.Core.Unify.Prep
import Sift.Core.Unify.Util

data NodeDegreeInfo = NodeDegreeInfo
  { _upPower :: Int
  , _downPower :: Int
  , _root :: Int
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
makeLenses ''NodeDegreeInfo

data AspectTag = Perfect | Imperfect | Frees
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

powerTag :: Int -> AspectTag
powerTag x | x > 0 = Frees
powerTag x | x == 0 = Perfect
powerTag x | x < 0 = Imperfect
type Aspect = (Int, AspectTag)
getAspects :: (Eq term, Ord term) => (Graph gr) => [term] -> [term] -> (Graphic gr (Portion term) b) -> Map (Portion term) Aspect
getAspects fUp fDown graphic =
  let
    nodes0 = graphic ^.. Extra.Graphic.nodes . seeMapImg . each . seeImgTup
    nodes1 = (\(term, index) -> (term, NodeDegreeInfo (freePower fUp' term) (freePower fDown' term) (boundRoot graphic term))) <$> nodes0
    nodes2 = (\(term, NodeDegreeInfo upI downI rootI) -> (term, ((upI + downI) - rootI))) <$> nodes1
    nodes3 = (\(term, power) -> (term, (power, powerTag power))) <$> nodes2
   in
    toMap nodes3
 where
  fUp' = TopSide <$> fUp
  fDown' = BottomSide <$> fDown
  frees = fUp' <> fDown'
