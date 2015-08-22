module StringUtilsSpec where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "StringUtils" $ do
    context "enlarge" $ do
      it "enlarges a string by a given factor" $ do
        enlarge 5 "fo" `shouldBe` "fo   "
