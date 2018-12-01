module Year2018.Day2Spec(day2specs) where

import Test.Hspec

day2specs :: IO ()
day2specs = hspec $ do

  describe "Day 2" $ do

    it "returns something" $
     True `shouldBe` True
