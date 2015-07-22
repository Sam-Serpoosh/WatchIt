module PaymentTrackerTest where

import Testing
import PaymentTracker

-- Sample data

payments :: [Payment]
payments = [
            Payment { value = 10, category = food,           description = "chicken shawerma" },
            Payment { value = 11, category = transportation, description = "uber to work"     },
            Payment { value = 20, category = food,           description = "Noon o Kabab"     }
           ]

-- Tests for totalPayment

calculatesTotalPayment :: [Payment] -> Double -> TestResult
calculatesTotalPayment payments expectedTotal = assertEqual expectedTotal (totalPayment payments)

-- Tests for paymentsPerCategory testing the food payments for this test

groupsPaymentsByCategory :: [Payment] -> (Category, [Payment]) -> TestResult
groupsPaymentsByCategory payments expectedCatPays = assertEqual expectedCatPays ((paymentsPerCategory payments) !! 0)

-- Tests for totalPaysPerCategory

calculatesTotalPerCategory :: [Payment] -> [(Category, Double)] -> TestResult
calculatesTotalPerCategory payments expectedTotalPerCat = assertEqual expectedTotalPerCat (totalPaysPerCategory payments)

-- Test for percentPerCategory
calculatesPercentPerCategory :: [Payment] -> [(Category, Double)] -> TestResult
calculatesPercentPerCategory payments expectedPercentPerCat = assertEqual expectedPercentPerCat (percentPerCategory payments)

-- Tests for presentableChartForCats
shapesPresentableChartData :: [(Category, Double)] -> [(Category, String)] -> TestResult
shapesPresentableChartData percentByCat expectedCatChart = assertEqual expectedCatChart (presentableChartForCats percentByCat)

main :: IO ()
main = do
  putStrLn $ show $ calculatesTotalPayment payments 41
  putStrLn $ show $ groupsPaymentsByCategory payments (food, [payments !! 0, payments !! 2])
  putStrLn $ show $ calculatesTotalPerCategory payments [(food, 30), (transportation, 11)]
  putStrLn $ show $ calculatesPercentPerCategory payments [(food, 73), (transportation, 26)]
  putStrLn $ show $ shapesPresentableChartData [(food, 47), (transportation, 52)] [(food, "####"), (transportation, "#####")]
