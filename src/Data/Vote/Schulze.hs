{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Vote.Schulze
  ( Edge
  , WeightedEdges
  , toWeightedEdges
  , directedWeightedEdges
  , mkGraph
  , mkIndex
  , toCandRef
  ) where

import           Algorithms.Schulze ( mkGraph', mkIndex, schulze )
import           Control.Monad.ST ( runST )
import           Data.Char ( chr, ord )
import qualified Data.Map.Strict as Map
import           Data.Tuple ( swap )
import           Data.Vector.Unboxed ( Vector, freeze )
import           Data.Vote.Types
                   ( CandidateRef, Edge, Vote, WeightedEdge, WeightedEdges )

toWeightedEdges :: [Vote] -> WeightedEdges
toWeightedEdges =
  foldr (\vs we -> toWeightedEdges' we (Map.assocs vs)) Map.empty

toWeightedEdges' ::
     WeightedEdges
  -> [(CandidateRef, Maybe Int)]
  -> WeightedEdges
toWeightedEdges' we [] = we
toWeightedEdges' we [_] = we
toWeightedEdges' we (v : vs) = foldr (toWeightedEdges'' v) we' vs
 where
  we' = toWeightedEdges' we vs

toWeightedEdges'' ::
     (CandidateRef, Maybe Int)
  -> (CandidateRef, Maybe Int)
  -> WeightedEdges
  -> WeightedEdges
toWeightedEdges'' (cand, mRank) (cand', mRank') =
  case (mRank, mRank') of
    (Nothing, Nothing) -> id
    (Just _, Nothing) -> Map.insertWith (\_ c -> c + 1) edge 1
    (Nothing, Just _) -> Map.insertWith (\_ c -> c + 1) edge' 1
    (Just r, Just r')
      | r < r' -> Map.insertWith (\_ c -> c + 1) edge 1
      | r' < r -> Map.insertWith (\_ c -> c + 1) edge' 1
      | otherwise -> id
 where
  edge = (fromCandRef cand, fromCandRef cand')
  edge' = swap edge

fromCandRef :: CandidateRef -> Int
fromCandRef c = ord c - ord 'A'

toCandRef :: Int -> CandidateRef
toCandRef i = chr (ord 'A' + i)

directedWeightedEdges :: WeightedEdges -> [WeightedEdge]
directedWeightedEdges we = Map.assocs $ Map.filterWithKey p we
 where
  p k cost = let mV = Map.lookup (swap k) we
             in  case mV of
                   Nothing -> True
                   Just cost' -> case compare cost cost' of
                     LT -> False
                     EQ -> False
                     GT -> True

mkGraph :: Int -> [((Int, Int), Int)] -> Vector Int
mkGraph n edges = runST $ do
  graph' <- mkGraph' n edges
  schulze n graph'
  freeze graph'
