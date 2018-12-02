module Year2018.Day1(day1star1, day1star2) where

import Data.List
import qualified Data.Set as S

data Accumulator = Accumulator Int (S.Set Int) deriving (Show, Read, Eq)

day1star1 :: [Int] -> Maybe Int
day1star1 xs = Just (current (last (scanList xs)))

day1star2 :: [Int] -> Maybe Int
day1star2 xs = fmap current $ firstDuplicate $ repeatInf xs

repeatInf :: [Int] -> [Int]
repeatInf [] = []
repeatInf xs = cycle xs

firstDuplicate :: [Int] -> Maybe Accumulator
firstDuplicate xs = find isDuplicate $ scanList xs

isDuplicate :: Accumulator -> Bool
isDuplicate (Accumulator current seen) = S.member current seen

scanList :: [Int] -> [Accumulator]
scanList xs = scanl scanNext initial xs

scanNext :: Accumulator -> Int -> Accumulator
scanNext (Accumulator current seen) change = Accumulator (current + change) (S.insert current seen)

initial :: Accumulator
initial = Accumulator 0 S.empty

current :: Accumulator -> Int
current (Accumulator c _) = c

seen :: Accumulator -> S.Set Int
seen (Accumulator _ s) = s
