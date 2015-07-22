module PaymentTrackerTest where

import Testing
import PaymentTracker

-- Sample data

payments :: [Payment]
payments = [
            Payment { value = 10, title = "dinner", category = food,           description = "chicken shawerma" },
            Payment { value = 11, title = "uber",   category = transportation, description = "uber to work" }
           ]

-- Tests for totalPayment

calculatesTotalPayment :: [Payment] -> Double -> TestResult
calculatesTotalPayment payments expectedTotal = assertEqual expectedTotal (totalPayment payments)

-- Tests for totalPerCategory

calculatesTotalPerCategory :: [Payment] -> [(Category, Double)] -> TestResult
calculatesTotalPerCategory payments expectedTotalPerCat = assertEqual expectedTotalPerCat (totalPerCategory payments)

-- Test for percentPerCategory
calculatesPercentPerCategory :: [Payment] -> [(Category, Double)] -> TestResult
calculatesPercentPerCategory payments expectedPercentPerCat = assertEqual expectedPercentPerCat (percentPerCategory payments)

main :: IO ()
main = do
  putStrLn $ show $ calculatesTotalPayment payments 21
  putStrLn $ show $ calculatesTotalPerCategory payments [(food, 10), (transportation, 11)]
  putStrLn $ show $ calculatesPercentPerCategory payments [(food, 47), (transportation, 52)]
