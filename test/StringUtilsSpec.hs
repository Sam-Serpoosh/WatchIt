module StringUtilsSpec where

import SpecHelper
import System.Time

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "StringUtils" $ do
    context "enlarge" $ do
      it "enlarges a string by a given factor" $ do
        enlarge 5 "fo" `shouldBe` "fo   "

    context "alignStrings" $ do
      it "align many strings based on the longest one" $ do
        let strs = ["hey", "hello", "i"]
        alignStrings strs  `shouldBe` ["hey  ", "hello", "i    "]

    context "getMonthByName" $ do
      it "returns month based on name" $ do
        getMonthByName "aug" `shouldBe` August

      it "returns January when not-existing name passed" $ do
        getMonthByName "bla" `shouldBe` January

    context "extractMonthAndYear" $ do
      it "extracts month and year values" $ do
        extractMonthAndYear "feb_2016_costs" `shouldBe` (February, 2016)

    context "calTimeToLabel" $ do
      it "shapes the label" $ do
        let calTime = createCalTime (January, 2016)
        calTimeToLabel calTime  `shouldBe` "January_2016"
