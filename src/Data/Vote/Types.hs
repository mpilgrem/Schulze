{-# LANGUAGE OverloadedStrings #-}

module Data.Vote.Types
  ( Candidates
  , CandidateRef
  , Vote
  , Edge
  , WeightedEdge
  , WeightedEdges
  , isValid
  ) where

import qualified Data.Map.Strict as Map
import           Data.Maybe ( isJust )
import qualified Data.Set as Set
import           Data.Text ( Text )
import qualified Data.Text.Compat as T
import           Data.Tuple.Extra ( first )

-- | Type synonym representing references to a candidates. Candidates are
-- referenced by letters \'A\', \'B'\, ...
type CandidateRef = Char

-- | Type synonym representing candidates.
type Candidates = Map.Map CandidateRef Text

-- | Type synonym representing votes. A vote is a collection of pairs of a
-- reference to a candidate and the candidate's (optional) rank (1, 2, ...).
type Vote = Map.Map CandidateRef (Maybe Int)

-- | Type synonym representing edges.
type Edge = (Int, Int)

-- | Type synonym representing weighted edges.
type WeightedEdge = (Edge, Int)

-- | Type synonym representing collections of weighted edges.
type WeightedEdges = Map.Map Edge Int

-- | If the vote is valid, given the candidates, yields Nothing. Otherwise
-- yields an error message.
isValid :: Candidates -> Vote -> Maybe Text
isValid candidates vote
  | not $ Set.null diffs = Just $
      "Vote includes invalid candidate reference(s): " <> T.show diffs <> "."
  | otherwise = isValid' 0 1 vote
 where
  n = Map.size candidates
  refs = Set.fromList $ take n ['A' ..]
  diffs = Set.difference (Map.keysSet vote) refs
  isValid' :: Int -> Int -> Vote -> Maybe Text
  isValid' c i vote'
    | i > n = Just $
        "Seeking rank " <> T.show i <> " when " <> T.show n <> "candidate(s)."
    | hasNoRanks = Nothing
    | otherwise = if c' == 0
        then Just $ "No candidate ranked " <> T.show i <> "."
        else isValid' (c + c') (i + c') vote''
   where
    hasNoRanks = Map.null $ Map.filter isJust vote'
    (c', vote'') = first Map.size $ Map.partition (Just i ==) vote'
