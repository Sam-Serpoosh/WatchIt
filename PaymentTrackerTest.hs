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
calculatesTotalPayment payments expectedTotal = assertEqual (totalPayment payments) expectedTotal

-- Tests for totalPerCategory

calculatesTotalPerCategory :: [Payment] -> [(Category, Double)] -> TestResult
calculatesTotalPerCategory payments expectedTotalPerCat = assertEqual (totalPerCategory payments) expectedTotalPerCat

main :: IO ()
main = do
  putStrLn $ show $ calculatesTotalPayment payments 21
  putStrLn $ show $ calculatesTotalPerCategory payments [(food, 10), (transportation, 11)]
