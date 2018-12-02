{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Foldable
import Control.Monad.Except
import Data.String.Interpolate
import Year2018.Day1
import Util.FileReader
import Util.Puzzle

instance Show ParseError where
    show (ParseError message) = [i|Error while parsing the file: #{message}|]

stars :: [Puzzle]
stars =
  Puzzle 1 1 day1star1 :
  Puzzle 1 2 day1star2 :
  []

main :: IO ()
main = runPuzzles "input.txt" >>= showResults

runPuzzles :: FilePath -> IO (Either ParseError [PuzzleResult])
runPuzzles path = runExceptT (fmap (solvePuzzles stars) (readFrequencyChanges path))

showResults :: Either ParseError [PuzzleResult] -> IO ()
showResults = either print $ traverse_ print
