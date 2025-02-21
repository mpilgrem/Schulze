{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Exception ( throwIO )
import           Control.Monad ( forM_ )
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Text ( unpack )
import           Data.Vector.Unboxed ( Vector, (!) )
import           Data.Vote.IO ( decodeEither )
import           Data.Vote.Schulze
                   ( directedWeightedEdges, mkGraph, mkIndex, toCandRef
                   , toWeightedEdges
                   )
import           Data.Vote.Types ( Candidates, Vote, WeightedEdges )
import           System.Environment ( getArgs )
import           Text.Printf ( printf )

main :: IO ()
main = do
  args <- getArgs
  let fp = case args of
             -- The default is the Wikipedia example
             [] -> "exampleVote.yaml"
             (arg:_) -> arg
  bs <- BS.readFile fp
  case decodeEither bs of
    Left err -> throwIO err
    Right (candidates, votes) -> process candidates votes

-- | Helper function to process the list of votes given the candidates.
process :: Candidates -> [Vote] -> IO ()
process candidates votes = do
  let n = length candidates
      beats = toWeightedEdges votes
      edges = directedWeightedEdges beats
      graph = mkGraph n edges
  printCandidates candidates
  blankLine
  printf "Number of votes cast: %3d\n" $ length votes
  blankLine
  printBeats n beats
  blankLine
  printStrongestPath n graph
  blankLine
  printRanking n graph
  blankLine
 where
  blankLine = putStrLn ""


-- | Helper function to output list of candidates.
printCandidates :: Candidates -> IO ()
printCandidates candidates' = do
  title "Candidates"
  forM_ (Map.assocs candidates') $ \(index, candidate) ->
    putStrLn $ index : ':' : ' ' : unpack candidate

-- | Helper function to output beat count table.
printBeats :: Int -> WeightedEdges -> IO ()
printBeats n we = do
  title "Beats"
  heading n
  forM_ ([0 .. n - 1] :: [Int]) $ \i -> do
    putStr $ toCandRef i : ": "
    forM_ ([0 .. n - 1] :: [Int]) $ \j -> do
      case Map.lookup (i, j) we of
        Nothing -> putStr "    "
        Just cost -> printf "%4d" cost
    putStr "\n"

-- | Helper function to output strongest beat path table.
printStrongestPath :: Int -> Vector Int -> IO ()
printStrongestPath n graph = do
  title "Strongest Path"
  heading n
  forM_ ([0 .. n - 1] :: [Int]) $ \i -> do
    putStr $ toCandRef i : ": "
    forM_ ([0 .. n - 1] :: [Int]) $ \j -> do
      printf "%4d" (graph ! mkIndex n (i, j))
    putStr "\n"

-- | Helper function to output ranking table.
printRanking :: Int -> Vector Int -> IO ()
printRanking n graph = do
  title "Ranking"
  heading n
  forM_ ([0 .. n - 1] :: [Int]) $ \i -> do
    putStr $ toCandRef i : ": "
    forM_ ([0 .. n - 1] :: [Int]) $ \j -> do
      let cost = graph ! mkIndex n (i, j)
          cost' = graph ! mkIndex n (j, i)
      putStr $ case compare cost cost' of
        LT -> "    "
        EQ -> [' ', ' ', '=', toCandRef j]
        GT -> [' ', ' ', '>', toCandRef j]
    putStr "\n"

-- | Helper function to output a title.
title :: String -> IO ()
title s = do
  putStrLn s
  putStrLn $ replicate (length s) '-'
  putStrLn ""

-- | Helper function to output headings for tables.
heading :: Int -> IO ()
heading n = do
  putStr "   "
  forM_ ([0 .. n - 1] :: [Int]) $ \j -> printf "%4c" (toCandRef j)
  putStr "\n"
  putStr "   "
  putStr $ concat $ replicate n " ---"
  putStr "\n"
