{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Foldable
import Control.Monad.Except
import Data.String.Interpolate
import Year2018.Day1
import Year2018.Day2
import Util.FileReader
import Util.Puzzle

instance Show ParseError where
  show (ParseError message) = [i|Error while parsing the file: #{message}|]

stars :: [Puzzle]
stars =
  [Puzzle 1 1 day1star1, Puzzle 1 2 day1star2, Puzzle 2 1 day2star1, Puzzle 2 2 day2star2]

main :: IO ()
main = runPuzzles >>= showResults

runPuzzles :: IO (Either ParseError [PuzzleResult])
runPuzzles = runExceptT (fmap (solvePuzzles stars) readInputData)

showResults :: Either ParseError [PuzzleResult] -> IO ()
showResults = either print $ traverse_ print
