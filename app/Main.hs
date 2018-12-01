module Main where

import Data.Foldable
import Data.Maybe
import Data.List
import Text.Read
import Year2018.Day1

data Star = Star Int Int (Maybe Int) deriving (Read, Eq)

instance Show Star where
    show (Star day star result) = "Day " ++ show day ++ ", Star " ++ show star ++ ": " ++ show result

main :: IO ()
main = do inputData <- readInputData "input.txt"
          showResult inputData

showResult :: Either String [Int] -> IO ()
showResult (Left message) = putStrLn("Error while parsing the file: " ++ message)
showResult (Right inputData) = traverse_ putStar (stars inputData)

stars :: [Int] -> [Star]
stars inputData =
  Star 1 1 (day1star1 inputData) :
  Star 1 2 (day1star2 inputData) :
  []

readInputData :: FilePath -> IO (Either String [Int])
readInputData = fmap stringsToInts . readLines

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

stringsToInts :: [String] -> Either String [Int]
stringsToInts = traverse readPlusMinus

readPlusMinus :: String -> Either String Int
readPlusMinus ('+' : rest) = parseNumber "+" rest
readPlusMinus rest = parseNumber "" rest

parseNumber :: String -> String -> Either String Int
parseNumber prefix string = fromMaybe (Left ("Could not parse '" ++ prefix ++ string ++ "'")) (fmap Right (readMaybe string))

putStar :: Star -> IO ()
putStar star = putStrLn(show star)
