{-# LANGUAGE QuasiQuotes #-}

module Util.Puzzle(solvePuzzles, Puzzle (Puzzle), PuzzleResult) where

import Data.String.Interpolate

data Puzzle = Puzzle Int Int ([Int] -> Maybe Int)

data PuzzleResult = PuzzleSuccess Int Int Int | PuzzleFailure Int Int deriving (Read, Eq)

instance Show PuzzleResult where
  show (PuzzleSuccess day star result) = [i|Day #{day} Star #{star} -> #{result}|]
  show (PuzzleFailure day star)        = [i|Day #{day} Star #{star} -> Failure! No result was returned.|]

solvePuzzles :: [Puzzle] -> [Int] -> [PuzzleResult]
solvePuzzles puzzles frequencyChanges = [ solve star frequencyChanges | star <- puzzles ]

solve :: Puzzle -> [Int] -> PuzzleResult
solve (Puzzle day star puzzle) frequencyChanges =
  maybe (PuzzleFailure day star) (PuzzleSuccess day star) $ puzzle frequencyChanges