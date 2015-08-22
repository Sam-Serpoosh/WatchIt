module RendererSpec where

import SpecHelper

-- Expected Values 

expectedRenderingOfFoodPayments = "food:  30.0\nchicken shawerma -> 10.0\nNoon o Kabab -> 20.0\n" 
expectedRenderingOfWarnings     = "food -> 100.0\nclothes -> 200.0\n"

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
        renderPaymentsOfCategory [(food, [chicken, kebob]), (transportation, [uberToWork, uberToAirport])] food `shouldBe` expectedRenderingOfFoodPayments

    context "renderWarnings" $ do
      it "renders warnings based on payments" $ do
        renderWarnings (Just [Warn food 100, Warn clothes 200]) `shouldBe`  expectedRenderingOfWarnings

      it "renders empty string when there is NO warnings" $ do
        renderWarnings Nothing `shouldBe` emptyString
