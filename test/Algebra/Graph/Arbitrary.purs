module Algebra.Graph.Test.Arbitrary where

import Algebra.Graph
import Control.Monad
import Prelude
import Test.QuickCheck

import Algebra.Graph.Labelled as LG
import Algebra.Graph.Labelled.AdjacencyMap as LAM

-- | Generate an arbitrary labelled 'LAM.AdjacencyMap'. It is guaranteed
-- that the resulting adjacency map is 'consistent'.
arbitraryLabelledAdjacencyMap :: forall e a. Arbitrary a => Ord a => Eq e => Arbitrary e => Monoid e => Gen (LAM.AdjacencyMap e a)
arbitraryLabelledAdjacencyMap = LAM.fromAdjacencyMaps <$> arbitrary

instance arbitraryLAMAdjMap :: (Arbitrary a, Ord a, Eq e, Arbitrary e, Monoid e) => Arbitrary (LAM.AdjacencyMap e a) where
  arbitrary = arbitraryLabelledAdjacencyMap
  shrink g = shrinkVertices ++ shrinkEdges
    where
    shrinkVertices =
      let
        vertices = LAM.vertexList g
      in
        do
          v <- vertices
          pure (LAM.removeVertex v g)

    shrinkEdges =
      let
        edges = LAM.edgeList g
      in
        do
          Tuple _ (Tuple v w) <- edges
          pure (LAM.removeEdge v w g)

-- | Generate an arbitrary labelled 'LAM.Graph' value of a specified size.
arbitraryLabelledGraph :: forall e a. Arbitrary a => Arbitrary e => Gen (LG.Graph e a)
arbitraryLabelledGraph = sized expr
  where
  expr 0 = return LG.empty

  expr 1 = LG.vertex <$> arbitrary

  expr n = do
    label <- arbitrary
    left <- choose (Tuple 0 n)
    LG.connect label <$> expr left <*> expr (n - left)

instance arbLGGraph :: (Arbitrary a, Arbitrary e, Monoid e) => Arbitrary (LG.Graph e a) where
  arbitrary = arbitraryLabelledGraph
  shrink LG.Empty = []
  shrink (LG.Vertex _) = [ LG.Empty ]
  shrink (LG.Connect e x y) =
    [ LG.Empty, x, y, LG.Connect mempty x y ]
      ++ ( do
            Tuple x' y' <- shrink (Tuple x y)
            pure (LG.Connect e x' y')
        )

instance arbTree :: Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized go
    where
    go 0 = do
      root <- arbitrary
      return $ Node root []

    go n = do
      subTrees <- choose (Tuple 0 (n - 1))
      let
        subSize = (n - 1) `div` subTrees
      root <- arbitrary
      children <- replicateM subTrees (go subSize)
      return $ Node root children
  shrink (Node r fs) = do
    fs' <- shrink fs
    pure (Node r fs')

-- TODO: Implement a custom shrink method.
instance arbDoc :: Arbitrary s => Arbitrary (Doc s) where
  arbitrary = mconcat.map literal <$> arbitrary

instance arbDistance :: (Arbitrary a, Num a, Ord a) => Arbitrary (Distance a) where
  arbitrary = (\x -> if x < 0 then distance infinite else distance (unsafeFinite x)) <$> arbitrary

instance arbCapacity :: (Arbitrary a, Num a, Ord a) => Arbitrary (Capacity a) where
  arbitrary = (\x -> if x < 0 then capacity infinite else capacity (unsafeFinite x)) <$> arbitrary

instance arbCount :: (Arbitrary a, Num a, Ord a) => Arbitrary (Count a) where
  arbitrary = (\x -> if x < 0 then count infinite else count (unsafeFinite x)) <$> arbitrary
 -- instance arbMinimum :: Arbitrary a => Arbitrary (Minimum a) where --     arbitrary = frequency [(10, pure <$> arbitrary), (1, pure noMinimum)] -- instance arbPowerSet :: (Arbitrary a, Ord a) => Arbitrary (PowerSet a) where --     arbitrary = PowerSet <$> arbitrary -- instance (Arbitrary o, Arbitrary a) => Arbitrary (Optimum o a) where --     arbitrary = Optimum <$> arbitrary <*> arbitrary -- instance (Arbitrary a, Arbitrary b, Ord a, Ord b) => Arbitrary (BAM.AdjacencyMap a b) where --     arbitrary = BAM.toBipartite <$> arbitrary --     shrink = map BAM.toBipartite . shrink . BAM.fromBipartite -- instance (Arbitrary a, Arbitrary b) => Arbitrary (BAM.List a b) where --     arbitrary = sized go --       where --         go 0 = return BAM.Nil --         go 1 = do h <- arbitrary --                   return $ BAM.Cons h BAM.Nil --         go n = do f <- arbitrary --                   s <- arbitrary --                   (BAM.Cons f . BAM.Cons s) <$> go (n - 2)