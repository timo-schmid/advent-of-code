module Year2018.Day2(
      day2star1
    , day2star2
    , countLetter
    , hasLetterCount
    , withLetterCount
    , checksumMultiForCount
    , combineAll
    , combineAllSet
    , allIdsWithDifference
    , mostSimilarIds
    , commonLetters) where

import Control.Monad
import Control.Applicative
import Data.Function
import qualified Data.List as L
import qualified Data.Set.Monad as S
import Data.Tuple.Select
import Util.FileReader

day2star1 :: InputData -> Maybe String
day2star1 (InputData _ ids) = Just (show (checksum ids))

day2star2 :: InputData -> Maybe String
day2star2 (InputData _ ids) = Just (commonLetters (mostSimilarIds (allIdsWithDifference ids)))

checksum :: [String] -> Int
checksum ids = checksumMultiForCount 2 ids * checksumMultiForCount 3 ids

checksumMultiForCount :: Int -> [String] -> Int
checksumMultiForCount num ids = S.size (withLetterCount num ids)

withLetterCount :: Int -> [String] -> S.Set String
withLetterCount count strings = do char <- S.fromList allLetters
                                   S.fromList (L.filter (hasLetterCount count char) strings)

hasLetterCount :: Int -> Char -> String -> Bool
hasLetterCount num char string = num == countLetter char string

countLetter :: Char -> String -> Int
countLetter c = count (== c)

count :: (a -> Bool) -> [a] -> Int
count pred = length . Prelude.filter pred

allLetters :: String
allLetters = ['a' .. 'z']

commonLetters :: (String, String, Int) -> String
commonLetters t = fst <$> L.filter (uncurry (==)) (zip (sel1 t) (sel2 t))

mostSimilarIds :: S.Set (String, String, Int) -> (String, String, Int)
mostSimilarIds = L.minimumBy (compare `on` sel3)

allIdsWithDifference :: [String] -> S.Set (String, String, Int)
allIdsWithDifference ids = fmap differenceBetweenIds (combineAllSet ids)

combineAllSet :: [String] -> S.Set (String, String)
combineAllSet xs = S.fromList $ fmap sortTuple (combineAll xs)

sortTuple :: (String, String) -> (String, String)
sortTuple t = if sel1 t < sel2 t then t else (sel2 t, sel1 t)

combineAll :: [String] -> [(String, String)]
combineAll xs = L.filter (uncurry (/=)) ((,) <$> xs <*> xs)

differenceBetweenIds :: (String, String) -> (String, String, Int)
differenceBetweenIds ids = (sel1 ids, sel2 ids, count (uncurry (/=)) (uncurry zip ids))
