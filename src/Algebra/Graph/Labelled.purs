module Algebra.Graph.Labelled where

import Prelude
import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Labelled.AdjacencyMap as AdjacencyMap

data Graph e a
  = Empty
  | Vertex a
  | Connect e (Graph e a) (Graph e a)

derive instance functorLabelledGraph :: Functor (Graph e)

instance showLabelledGraph :: (Show e, Show a) => Show (Graph e a) where
  show = case _ of
    Empty -> "empty"
    Vertex v -> "vertex " <> show v
    Connect e gl gr -> show gl <> " <- " <> show e <> " -> " <> show gr

instance eqLabelledGraph :: (Eq e, Monoid e, Ord a) => Eq (Graph e a) where
  eq x y = eq (toAdjacencyMap x) (toAdjacencyMap y)

toAdjacencyMap :: forall e a. Eq e => Monoid e => Ord a => Graph e a -> AdjacencyMap e a
toAdjacencyMap = foldg AdjacencyMap.empty AdjacencyMap.vertex AdjacencyMap.connect

fromAdjacencyMap :: forall e a. Monoid e => AdjacencyMap e a -> Graph e a
fromAdjacencyMap = overlays <<< map go <<< Map.toUnfoldable <<< AdjacencyMap.adjacencyMap
  where
    go (Tuple u m) = overlay (vertex u) (edges [ (e, u, v) | (v, e) <- Map.toList m])

foldg :: forall e a b. b -> (a -> b) -> (e -> b -> b -> b) -> Graph e a -> b
foldg e v c = go
  where
  go Empty = e
  go (Vertex x) = v x
  go (Connect e' x y) = c e' (go x) (go y)
