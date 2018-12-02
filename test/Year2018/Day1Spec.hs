module Year2018.Day1Spec(day1specs) where

import Test.Hspec
import Year2018.Day1
import Util.Puzzle
import Util.FileReader

day1specs :: IO ()
day1specs = hspec $

  describe "Day 1" $ do

    it "returns the initial frequency for []" $
      day1star1 (InputData [] []) `shouldBe` Just "0"

    it "returns the correct sum for frequencies +1 +1 +1" $
      day1star1 (InputData [1, 1, 1] []) `shouldBe` Just "3"

    it "returns the correct sum for frequencies +1 +1 -2" $
      day1star1 (InputData [1, 1, -2] []) `shouldBe` Just "0"

    it "returns the correct sum for frequencies [ -1 -2 -3 ]" $
      day1star1 (InputData [-1, -2, -3] []) `shouldBe` Just "-6"

    it "returns nothing for frequencies []" $
      day1star2 (InputData [] []) `shouldBe` Nothing

    it "returns the first frequency reached twice for frequencies [ +1 -1 ]" $
      day1star2 (InputData [1, -1] []) `shouldBe` Just "0"

    it "returns the first frequency reached twice for frequencies [ +3 +3 +4 -2 -4 ]" $
      day1star2 (InputData [3, 3, 4, -2, -4] []) `shouldBe` Just "10"

    it "returns the first frequency reached twice for frequencies [ -6 +3 +8 +5 -6 ]" $
      day1star2 (InputData [-6, 3, 8, 5, -6] []) `shouldBe` Just "5"

    it "returns the first frequency reached twice for frequencies [ +7 +7 -2 -7 -4 ]" $
      day1star2 (InputData [7, 7, -2, -7, -4] []) `shouldBe` Just "14"
