module RendererSpec where

import SpecHelper

-- Values for Specs

expectedRenderingOfFoodPayments = "food:  30.0\nchicken shawerma -> 10.0\nNoon o Kabab -> 20.0\n" 
expectedRenderingOfWarnings     = "food -> 100.0\nclothes -> 200.0\n"
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
