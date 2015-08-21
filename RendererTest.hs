module RendererTest where

import StringUtils
import Testing
import TestData
import Category
import Renderer
import PaymentTracker (Payment, Warning(..))

-- Tests for presentableChartForCats

shapesPresentableChartData :: [(Category, Double)] -> [String] -> TestResult
shapesPresentableChartData percentByCat expectedChartPixels = assertEqual expectedChartPixels (map snd $ presentableChartForCats percentByCat)

-- Tests for renderPaymentsOfCategory

rendersPaymentsOfCategory :: [(Category, [Payment])] -> String -> TestResult
rendersPaymentsOfCategory paysPerCat expectedPresentation = assertEqual expectedPresentation (renderPaymentsOfCategory paysPerCat food)

-- Tests for renderWarnings

rendersWarningsIfExist :: Maybe [Warning] -> String -> TestResult
rendersWarningsIfExist warns expectedWarnsRendering = assertEqual expectedWarnsRendering (renderWarnings warns)

-- Expected Values 

expectedRenderingOfFoodPayments = "food:  30.0\nchicken shawerma -> 10.0\nNoon o Kabab -> 20.0\n" 
expectedRenderingOfWarnings     = "food -> 100.0\nclothes -> 200.0\n"

main :: IO ()
main = do
  putStrLn $ show $ shapesPresentableChartData [(food, 47), (transportation, 52)] ["#####", "######"]
  putStrLn $ show $ rendersPaymentsOfCategory [(food, [chicken, kebob]), (transportation, [uberToWork, uberToAirport])] expectedRenderingOfFoodPayments
  putStrLn $ show $ rendersWarningsIfExist (Just [Warn food 100, Warn clothes 200]) expectedRenderingOfWarnings
  putStrLn $ show $ rendersWarningsIfExist Nothing emptyString
