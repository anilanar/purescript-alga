module Algebra.Graph.Labelled where

import Prelude
import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Labelled.AdjacencyMap as AdjacencyMap
import Data.Foldable (foldr)
import Data.Map as Map
import Data.Tuple (Tuple(..))

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
  go (Tuple u m) =
    overlay (vertex u)
      ( edges
          $ do
              Tuple v e <- Map.toUnfoldable m
              pure (Tuple e (Tuple u v))
      )

foldg :: forall e a b. b -> (a -> b) -> (e -> b -> b -> b) -> Graph e a -> b
foldg e v c = go
  where
  go Empty = e

  go (Vertex x) = v x

  go (Connect e' x y) = c e' (go x) (go y)

connect :: forall e a. e -> Graph e a -> Graph e a -> Graph e a
connect = Connect

vertex :: forall e a. a -> Graph e a
vertex = Vertex

edge :: forall e a. e -> a -> a -> Graph e a
edge e x y = connect e (vertex x) (vertex y)

overlay :: forall e a. Monoid e => Graph e a -> Graph e a -> Graph e a
overlay = connect mempty

empty :: forall e a. Graph e a
empty = Empty

overlays :: forall e a. Monoid e => Array (Graph e a) -> Graph e a
overlays = foldr overlay empty

edges :: forall e a. Monoid e => Array (Tuple e (Tuple a a)) -> Graph e a
edges = overlays <<< map (\(Tuple e (Tuple x y)) -> edge e x y)

isEmpty :: forall e a. Graph e a -> Boolean
isEmpty = foldg true (const false) (const (&&))