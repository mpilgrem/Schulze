module Data.Vote.Schulze
  ( Edge
  , WeightedEdges
  , toWeightedEdges
  , directedWeightedEdges
  , mkGraph
  , mkIndex
  ) where

import           Control.Monad ( forM_, unless, when )
import           Control.Monad.ST ( ST, runST )
import qualified Data.Map.Strict as Map
import           Data.Tuple ( swap )
import           Data.Vector.Unboxed ( Vector, freeze )
import           Data.Vector.Unboxed.Mutable ( MVector )
import qualified Data.Vector.Unboxed.Mutable as V
import           Data.Vote.Types
                   ( Edge, Ranking, Vote, WeightedEdge, WeightedEdges, toEdge )

toWeightedEdges :: [Vote] -> WeightedEdges
toWeightedEdges =
  foldr (\vs we -> addRankings we (Map.assocs vs)) Map.empty

addRankings :: WeightedEdges -> [Ranking] -> WeightedEdges
addRankings we [] = we
addRankings we [_] = we
addRankings we (r : rs) = foldr (addRankingPair r) we' rs
 where
  we' = addRankings we rs

addRankingPair :: Ranking -> Ranking -> WeightedEdges -> WeightedEdges
addRankingPair (cand, mRank) (cand', mRank') = case (mRank, mRank') of
  (Nothing, Nothing) -> id
  (Just _, Nothing) -> addBeat edge
  (Nothing, Just _) -> addBeat edge'
  (Just r, Just r')
    | r < r' -> addBeat edge
    | r' < r -> addBeat edge'
    | otherwise -> id
 where
  edge = toEdge cand cand'
  edge' = swap edge
  addBeat e = Map.insertWith (\_ c -> c + 1) e 1

directedWeightedEdges :: WeightedEdges -> [WeightedEdge]
directedWeightedEdges we = Map.assocs $ Map.filterWithKey p we
 where
  p k cost = case Map.lookup (swap k) we of
               Nothing -> True
               Just cost' -> case compare cost cost' of
                 LT -> False
                 EQ -> False
                 GT -> True

--------------------------------------------------------------------------------
-- Implementation of Schulze widest path algorithm.
--
-- See Wikipedia article for details:
-- https://en.wikipedia.org/wiki/Schulze_method
--
-- Based on package hgeometry-combinatorial-0.14
--------------------------------------------------------------------------------

-- | Compute the index of an element in a given range.
mkIndex :: Int -> Edge -> Int
mkIndex n (i, j) = i * n + j

-- | Construct a weighted graph from \(n\) vertices and a list of weighted edges.
mkGraph :: Int -> [WeightedEdge] -> Vector Int
mkGraph n edges = runST $ do
  graph <- V.replicate (n * n) 0
  forM_ edges $ \(edge, cost) -> V.unsafeWrite graph (mkIndex n edge) cost
  schulze n graph
  freeze graph

schulze :: Int -> MVector s Int -> ST s ()
schulze n graph = do
  when (n * n /= V.length graph) $ error "Bad bounds"
  forM_ [0 .. n - 1] $ \i ->
    forM_ [0 .. n - 1] $ \j -> do
      unless (i == j) $ do
        forM_ [0 .. n - 1] $ \k -> do
          unless ( i == k || j == k) $ do
            costJK <- access (j, k)
            costJI <- access (j, i)
            costIK <- access (i, k)
            let cost = max costJK (min costJI costIK)
            put (j, k) cost
 where
  access idx = V.unsafeRead graph (mkIndex n idx)
  put idx e = V.unsafeWrite graph (mkIndex n idx) e
