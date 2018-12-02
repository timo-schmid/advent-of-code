module Year2018.Day2Spec(day2specs) where

import Control.Monad.Except
import qualified Data.Set.Monad as S
import Year2018.Day2
import Test.Hspec
import Util.FileReader

testIds :: [String]
testIds = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]

day2specs :: IO ()
day2specs = hspec $ describe "Day 2" $ do

     it "returns correct number of contained a's" $
       countLetter 'a' "bababc" `shouldBe` 2

     it "returns correct number of contained b's" $
       countLetter 'b' "bababc" `shouldBe` 3

     it "returns correct number of contained c's" $
       countLetter 'c' "bababc" `shouldBe` 1

     it "checks the correct number of contained a's" $
       hasLetterCount 2 'a' "bababc" `shouldBe` True

     it "checks the correct number of contained b's" $
       hasLetterCount 3 'b' "bababc" `shouldBe` True

     it "checks the correct number of contained c's" $
       hasLetterCount 1 'c' "bababc" `shouldBe` True

     it "checks the incorrect number of contained a's" $
       hasLetterCount 5 'a' "bababc" `shouldBe` False

     it "checks the incorrect number of contained b's" $
       hasLetterCount 0 'b' "bababc" `shouldBe` False

     it "checks the incorrect number of contained c's" $
       hasLetterCount 10 'c' "bababc" `shouldBe` False

     it "returns the correct strings that contain a letter 2 times" $
       withLetterCount 2 testIds `shouldBe` S.fromList [ "bababc", "abbcde", "aabcdd", "abcdee" ]

     it "returns the correct strings that contain a letter 3 times" $
       withLetterCount 3 testIds `shouldBe` S.fromList [ "bababc", "abcccd", "ababab" ]

     it "returns the correct multiplier for 2 letters" $
       checksumMultiForCount 2 testIds `shouldBe` 4

     it "returns the correct multiplier for 3 letters" $
       checksumMultiForCount 3 testIds `shouldBe` 3

     it "returns the correct computed checksum for the testdata" $
       day2star1 (InputData [] testIds) `shouldBe` Just "12"

     it "combines all ids together" $
       combineAll [ "a", "b", "c" ] `shouldBe` [ ("a", "b"), ("a", "c"), ("b", "a"), ("b", "c"), ("c", "a"), ("c", "b")]

     it "combines all ids together without duplicates" $
       combineAllSet [ "a", "b", "c" ] `shouldBe` S.fromList [ ("a", "b"), ("a", "c"), ("b", "c")]

     it "combines all ids together without duplicates with their difference" $
       allIdsWithDifference [ "aaaa", "bbbb", "cccc", "abcc" ] `shouldBe` S.fromList [
         ("aaaa", "bbbb", 4), ("aaaa", "cccc", 4), ("aaaa", "abcc", 3),
         ("abcc", "bbbb", 3), ("abcc", "cccc", 2),
         ("bbbb", "cccc", 4)
       ]

     it "finds the most similar ids" $
       mostSimilarIds (allIdsWithDifference testIds) `shouldBe` ("abcdee", "abcdef", 1)

     it "finds the common letters of two ids" $
       commonLetters ("abcdee", "abcdef", 1) `shouldBe` "abcde"
