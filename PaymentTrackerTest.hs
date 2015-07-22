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

main :: IO ()
main = putStrLn $ show $ calculatesTotalPayment payments 21
