{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Extra.Graphic where

import Data.Graph.Inductive hiding (getNode)
import Extra.Lens
import Extra.Map

type NodeStore a = Map a Int
data Graphic gr a b = Graphic
  { _gr :: gr () b
  , _nodes :: NodeStore a
  }

makeLenses ''Graphic

mkGraphic :: (Graph gr) => gr () b -> NodeStore a -> Graphic gr a b
mkGraphic = Graphic
highestIndex :: NodeStore a -> Int
highestIndex store = maximum $ getValues store
getIndex :: (Eq a) => (a, NodeStore a) -> (Int, NodeStore a)
getIndex (node, store) =
  case mlookup store node of
    Just i -> (i, store)
    Nothing ->
      let
        high = highestIndex store + 1
        store' = store <> (toMap1 (node, high))
       in
        (high, store')
getNode :: (Int, NodeStore a) -> Maybe (a, NodeStore a)
getNode (index, store) =
  case mlookupV store index of
    Just i -> Just (i, store)
    Nothing -> Nothing

makeEdges :: (Eq a) => (Map a a, NodeStore a) -> (Map Int Int, NodeStore a)
makeEdges (map, store) = case fromMap map of
  ((x, y) : m) ->
    let
      (x1, store1) = getIndex (x, store)
      (y1, store2) = getIndex (y, store1)
      (m1, store3) = makeEdges (toMap m, store2)
     in
      ((toMap1 (x1, y1)) <> m1, store)
  _ -> (mempty, store)
newGraph :: (Eq a, Graph gr) => Map a a -> Graphic gr a ()
newGraph map =
  let
    (map', store) = (makeEdges (map, mempty))
   in
    mkGraphic (mkUGraph [0 .. (highestIndex store)] (fromMap map')) store
instance (Graph gr, Show a, Show b) => Show (Graphic gr a b) where
  show (Graphic gr nmap) =
    "\nNodes: "
      ++ show (nmap)
      ++ "\nGraph: "
      ++ prettify gr
      ++ "\n"
