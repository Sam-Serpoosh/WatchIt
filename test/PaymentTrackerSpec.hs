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

    context "Paid Money for Each Category over Months" $ do
      let jan = "jan_2016"
      let feb = "feb_2016"
      let foodJan = (food, jan, 20)
      let foodFeb = (food, feb, 30)

      context "totalPaidInMonthCategories" $ do
        it "creates triples of (category, month, paid)" $ do
          let monthPayments = (jan, payments)
          totalPaidInMonthCategories monthPayments `shouldBe` [(food, jan, 30), (transportation, jan, 11)]

      context "groupMonthsPaymentsByCat" $ do
        it "group payments of months by their category value" $ do
          let monthsPaidCats = [foodJan, (transportation, jan, 50), foodFeb]
          groupMonthsPaymentsByCat monthsPaidCats `shouldBe` [[foodJan, foodFeb], [(transportation, jan, 50)]]

      context "factorOutCatMonthsPayments" $ do
        it "factors paid value for a category in different months" $ do
          let monthsPays = [(food, jan, 20), (food, feb, 30)]
          factorOutCatMonthsPayments monthsPays `shouldBe` (food, [(jan, 20), (feb, 30)])

      context "categoryPaidOverMonths" $ do
        it "collects value paid in different months for each category" $ do
          let monthsPayments = [(jan, [chicken, uberToWork, kebob]), (feb, [uberToAirport, kebob, chicken, kebob])]
          categoryPaidOverMonths monthsPayments `shouldBe` [(food, [(jan, 30), (feb, 50)]), (transportation, [(jan, 11), (feb, 60)])]
