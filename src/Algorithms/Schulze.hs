--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Schulze
--
-- Implementation of Schulze widest path algorithm.
--
-- See Wikipedia article for details:
-- https://en.wikipedia.org/wiki/Schulze_method
--
-- Based on package hgeometry-combinatorial-0.14
--------------------------------------------------------------------------------
module Algorithms.Schulze
  ( mkIndex
  , mkGraph'
  , schulze
  ) where

import           Control.Monad ( forM_, unless, when )
import           Control.Monad.ST ( ST )
import           Data.Vector.Unboxed.Mutable as V
                   ( MVector, Unbox, length, replicate, unsafeRead, unsafeWrite
                   )

schulze :: (Ord a, Unbox a) => Int -> MVector s a -> ST s ()
schulze n graph = do
    let nSq = V.length graph
    when (n * n /= nSq) $ error "Bad bounds"
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

-- | Compute the index of an element in a given range.
mkIndex :: Int -> (Int, Int) -> Int
mkIndex n (i, j) = i * n + j

-- | Construct a weighted graph from \(n\) vertices and a list of weighted edges.
mkGraph' :: (Unbox a, Num a) => Int -> [((Int, Int), a)] -> ST s (MVector s a)
mkGraph' n edges = do
  graph <- V.replicate (n*n) (fromInteger 0)
  forM_ edges $ \(edge, cost) -> unsafeWrite graph (mkIndex n edge) cost
  pure graph
