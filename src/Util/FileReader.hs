{-# LANGUAGE QuasiQuotes #-}

module Util.FileReader(
  readInputData,
  InputData (InputData),
  ErrorIO,
  ParseError (ParseError)
) where

import Control.Applicative
import Control.Monad.Except
import Data.List
import Data.Either
import Data.Maybe
import Data.String.Interpolate
import Text.Read

data InputData = InputData [Int] [String] deriving (Read, Eq)

class AccessInputData inputData where
  day1 :: inputData -> [Int]
  day2 :: inputData -> [String]

instance AccessInputData InputData where
  day1 (InputData day1 _) = day1
  day2 (InputData _ day2) = day2

instance Show InputData where
  show (InputData day1 day2) = [i|InputData(day1=#{length day1}, day2 = #{length day2})|]

newtype ParseError = ParseError String deriving (Read, Eq)

type ErrorIO = ExceptT ParseError IO

readInputData :: ErrorIO InputData
readInputData = do day1 <- readFrequencyChanges "input-day1.txt"
                   day2 <- readLines "input-day2.txt"
                   return $ InputData day1 day2

readFrequencyChanges :: FilePath -> ErrorIO [Int]
readFrequencyChanges path = readLines path >>= traverse readPlusMinus

readLines :: FilePath -> ErrorIO [String]
readLines path = liftIO (lines <$> readFile path)

readPlusMinus :: String -> ErrorIO Int
readPlusMinus ('+' : rest) = parseNumber "+" rest
readPlusMinus rest         = parseNumber [] rest

parseNumber :: String -> String -> ErrorIO Int
parseNumber prefix string =
  maybe (parseError [i|#{prefix}#{string}|]) return (readMaybe string)

parseError :: String -> ErrorIO Int
parseError msg = throwError $ ParseError [i|Could not parse '#{msg}'|]