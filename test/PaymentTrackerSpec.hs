module PaymentTrackerSpec where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PaymentTracker" $ do
    context "totalPaid" $ do
      it "calculates total money paid" $ do
        totalPaid payments `shouldBe` 41

    context "paymentsPerCategory" $ do
      it "groups payments per category" $ do
        ((paymentsPerCategory payments) !! 0) `shouldBe` (food, [payments !! 0, payments !! 2])

    context "totalPaidPerCategory" $ do
      it "calculates total money paid per category" $ do
        totalPaidPerCategory payments `shouldBe` [(food, 30), (transportation, 11)]

    context "percentPerCategory" $ do
      it "calculates percentage associated with each category" $ do
        percentPerCategory payments `shouldBe` [(food, 74), (transportation, 27)]

    context "warnings" $ do
      it "detects warnings for over-payments" $ do
        warnings overPayments `shouldBe` (Just [Warn clothes 100])

      it "knows when there is NO warning required" $ do
        warnings payments `shouldBe` Nothing
