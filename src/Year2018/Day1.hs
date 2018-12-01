module Year2018.Day1(day1star1, day1star2) where

import Data.List
import qualified Data.Set as Set

day1star1 :: [Int] -> Maybe Int
day1star1 xs = Just (current (last (scanList xs)))

day1star2 :: [Int] -> Maybe Int
day1star2 [] = Nothing
day1star2 xs = fmap current (firstDuplicate (cycle xs))

data Accumulator = Accumulator Int (Set.Set Int) deriving (Show, Read, Eq)

firstDuplicate :: [Int] -> Maybe Accumulator
firstDuplicate xs = find isDuplicate (scanList xs)

isDuplicate :: Accumulator -> Bool
isDuplicate (Accumulator current seen) = Set.member current seen

scanList :: [Int] -> [Accumulator]
scanList list = scanl scanNext initial list

scanNext :: Accumulator -> Int -> Accumulator
scanNext (Accumulator current seen) change = Accumulator (current + change) (Set.insert current seen)

initial :: Accumulator
initial = Accumulator 0 Set.empty

current :: Accumulator -> Int
current (Accumulator c _) = c

seen :: Accumulator -> Set.Set Int
seen (Accumulator _ s) = s
