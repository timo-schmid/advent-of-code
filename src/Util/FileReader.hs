{-# LANGUAGE QuasiQuotes #-}

module Util.FileReader(readFrequencyChanges, ErrorIO, ParseError (ParseError)) where

import Control.Monad.Except
import Data.Either
import Data.Maybe
import Data.String.Interpolate
import Text.Read

data ParseError = ParseError String deriving (Read, Eq)

type ErrorIO = ExceptT ParseError IO

readFrequencyChanges :: FilePath -> ErrorIO [Int]
readFrequencyChanges path = readLines path >>= traverse readPlusMinus

readLines :: FilePath -> ErrorIO [String]
readLines path = liftIO $ fmap lines $ readFile path

readPlusMinus :: String -> ErrorIO Int
readPlusMinus ('+' : rest) = parseNumber "+" rest
readPlusMinus rest         = parseNumber [] rest

parseNumber :: String -> String -> ErrorIO Int
parseNumber prefix string =
  maybe (parseError [i|#{prefix}#{string}|]) return (readMaybe string)

parseError :: String -> ErrorIO Int
parseError msg = throwError $ ParseError [i|Could not parse '#{msg}'|]