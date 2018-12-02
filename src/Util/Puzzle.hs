{-# LANGUAGE QuasiQuotes #-}

module Util.Puzzle(solvePuzzles, Puzzle (Puzzle), PuzzleResult) where

import Data.String.Interpolate
import Util.FileReader

data Puzzle = Puzzle Int Int (InputData -> Maybe String)

data PuzzleResult = PuzzleSuccess Int Int String | PuzzleFailure Int Int deriving (Read, Eq)

instance Show PuzzleResult where
  show (PuzzleSuccess day star result) = [i|Day #{day} Star #{star} -> #{result}|]
  show (PuzzleFailure day star)        = [i|Day #{day} Star #{star} -> Failure! No result was returned.|]

solvePuzzles :: [Puzzle] -> InputData -> [PuzzleResult]
solvePuzzles puzzles inputData = [ solve star inputData | star <- puzzles ]

solve :: Puzzle -> InputData -> PuzzleResult
solve (Puzzle day star puzzle) inputData =
  maybe (PuzzleFailure day star) (PuzzleSuccess day star) $ puzzle inputData
