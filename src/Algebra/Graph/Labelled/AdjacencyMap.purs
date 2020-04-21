module Algebra.Graph.Labelled.AdjacencyMap where

import Prelude
import Control.Fold as Fold
import Data.Array (snoc, (:))
import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as S
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Internal.Map as IMap

newtype AdjacencyMap e a
  = AM (Map a (Map a e))

derive instance labelledAdjacencyMapNewtype :: Newtype (AdjacencyMap e a) _

derive newtype instance labelledAdjacencyMapEq :: (Eq e, Eq a) => Eq (AdjacencyMap e a)

derive newtype instance labelledAdjacencyMapShow :: (Show e, Show a) => Show (AdjacencyMap e a)

instance labelledAdjacencyMapOrd :: (Ord e, Monoid e, Ord a) => Ord (AdjacencyMap e a) where
  compare x y =
    Fold.foldl Fold.mconcat
      [ compare (vertexCount x) (vertexCount y)
      , compare (vertexSet x) (vertexSet y)
      , compare (edgeCount x) (edgeCount y)
      , compare (eSet x) (eSet y)
      , cmp
      ]
    where
    eSet = Set.map snd <<< edgeSet

    cmp
      | x == y = EQ
      | overlays [ x, y ] == y = LT
      | otherwise = compare x y

adjacencyMap :: forall e a. AdjacencyMap a e -> Map e (Map e a)
adjacencyMap (AM m) = m

vertexCount :: forall a e. AdjacencyMap e a -> Int
vertexCount = Map.size <<< adjacencyMap

vertexSet :: forall a e. AdjacencyMap e a -> Set a
vertexSet = Map.keys <<< adjacencyMap

edgeCount :: forall e a. AdjacencyMap e a -> Int
edgeCount = Fold.foldl Fold.sum <<< map Map.size <<< unwrap

edgeList :: forall e a. AdjacencyMap e a -> Array (Tuple e (Tuple a a))
edgeList (AM m) = do
  Tuple x ys <- Map.toUnfoldable m
  Tuple y e <- Map.toUnfoldable ys
  pure $ Tuple e (Tuple x y)

edgeSet :: forall e a. Ord a => Ord e => AdjacencyMap e a -> Set (Tuple e (Tuple a a))
edgeSet = S.fromFoldable <<< edgeList

overlays :: forall t11 t19 t5 t6. Ord t5 => Foldable t11 => Functor t11 => Newtype t19 (Map t5 (Map t5 t6)) => t11 t19 -> AdjacencyMap t6 t5
overlays = AM <<< IMap.unionsWith Map.union <<< map unwrap

empty :: forall e a. AdjacencyMap e a
empty = AM Map.empty

vertex :: forall e a. a -> AdjacencyMap e a
vertex x = AM $ Map.singleton x Map.empty

edge :: forall e a. Eq e => Monoid e => Ord a => e -> a -> a -> AdjacencyMap e a
edge e x y
  | e == mempty = vertices [ x, y ]
  | x == y = AM $ Map.singleton x (Map.singleton x e)
  | otherwise = AM $ Map.fromFoldable [ (Tuple x (Map.singleton y e)), Tuple y Map.empty ]

vertices :: forall e a. Ord a => Array a -> AdjacencyMap e a
vertices = AM <<< Map.fromFoldable <<< map \x -> (Tuple x Map.empty)

connect :: forall e a. Eq e => Monoid e => Ord a => e -> AdjacencyMap e a -> AdjacencyMap e a -> AdjacencyMap e a
connect e (AM x) (AM y)
  | e == mempty = overlay (AM x) (AM y)
  | otherwise = AM $ IMap.unionsWith IMap.nonZeroUnion $ [ x, y, combined ]
    where
    combined = Map.fromFoldable (toKeysArray x <#> (\xa -> Tuple xa submap))
    submap = Map.fromFoldable (toKeysArray y <#> (\ya -> Tuple ya e))
    toKeysArray :: forall k v. Map k v -> Array k
    toKeysArray = Set.toUnfoldable <<< Map.keys

overlay :: forall e a. Eq e => Monoid e => Ord a => AdjacencyMap e a -> AdjacencyMap e a -> AdjacencyMap e a
overlay (AM x) (AM y) = AM $ Map.unionWith IMap.nonZeroUnion x y