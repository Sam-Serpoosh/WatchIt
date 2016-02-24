module RendererSpec where

import SpecHelper

-- Expected Values for some Specs

expectedRenderingOfFoodPayments = "food:  30.0\nchicken shawerma -> 10.0\nNoon o Kabab     -> 20.0\n" 
expectedRenderingOfWarnings     = "food    -> 100.0\nclothes -> 200.0\n"
sampleCategoryPayments          = [(food, [chicken, kebob]), (transportation, [uberToWork, uberToAirport])]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Renderer" $ do
    context "presentableChartForCats" $ do
      it "shapes presentable chart data" $ do
        map snd (presentableChartForCats [(food, 47), (transportation, 52)]) `shouldBe` ["#####", "######"]

    context "renderPaymentsOfCategory" $ do
      it "renders payments of a category" $ do
        renderPaymentsOfCategory sampleCategoryPayments food `shouldBe` expectedRenderingOfFoodPayments

    context "renderWarnings" $ do
      it "renders warnings based on payments" $ do
        renderWarnings (Just [Warn food 100, Warn clothes 200]) `shouldBe`  expectedRenderingOfWarnings

      it "renders empty string when there is NO warnings" $ do
        renderWarnings Nothing `shouldBe` emptyString

    context "alignPayment" $ do
      it "aligns payment description based on given enlarging factor" $ do
        let payment = Payment { value = 10.0, category = fun, description = "movie" }
        alignPayment 8 payment `shouldBe` Payment { value = 10.0, category = fun, description = "movie   " }

    context "alignCategory" $ do
      it "aligns category name based on given enlarging factory" $ do
        let cat = Cat { name = "fun", threshold = 100.0 }
        alignCategory 5 cat `shouldBe` Cat { name = "fun  ", threshold = 100.0 }

    context "Bar Chart for money spent on a category in different months" $ do
      context "valueToPixel" $ do
        it "converts values to pixels for bar chart" $ do
          valueToPixel 188 `shouldBe` "####"

      context "barChartCatSpentMonths" $ do
        it "draws a bar chart for category payments in months" $ do
          let jan = "jan_2016"
          let aug = "august_2015"
          let catMonthsPays = (food, [(aug, 60), (jan, 120)])
          barChartCatSpentMonths catMonthsPays `shouldBe` "FOOD\naugust_2015: ##\njan_2016   : ###\n"
