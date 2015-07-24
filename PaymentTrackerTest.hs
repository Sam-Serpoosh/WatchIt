module PaymentTrackerTest where

import Testing
import TestData
import Category
import PaymentTracker

-- Tests for totalPaid

calculatesTotalPayment :: [Payment] -> Double -> TestResult
calculatesTotalPayment payments expectedTotal = assertEqual expectedTotal (totalPaid payments)

-- Tests for paymentsPerCategory testing the food payments for this test

groupsPaymentsByCategory :: [Payment] -> (Category, [Payment]) -> TestResult
groupsPaymentsByCategory payments expectedCatPays = assertEqual expectedCatPays ((paymentsPerCategory payments) !! 0)

-- Tests for totalPaidPerCategory

calculatesTotalPerCategory :: [Payment] -> [(Category, Double)] -> TestResult
calculatesTotalPerCategory payments expectedTotalPerCat = assertEqual expectedTotalPerCat (totalPaidPerCategory payments)

-- Test for percentPerCategory

calculatesPercentPerCategory :: [Payment] -> [(Category, Double)] -> TestResult
calculatesPercentPerCategory payments expectedPercentPerCat = assertEqual expectedPercentPerCat (percentPerCategory payments)

-- Tests for warnings

givesWarnings :: [Payment] -> Maybe [Warning] -> TestResult
givesWarnings payments expectedWarnings = assertEqual expectedWarnings (warnings payments)

main :: IO ()
main = do
  putStrLn $ show $ calculatesTotalPayment payments 41
  putStrLn $ show $ groupsPaymentsByCategory payments (food, [payments !! 0, payments !! 2])
  putStrLn $ show $ calculatesTotalPerCategory payments [(food, 30), (transportation, 11)]
  putStrLn $ show $ calculatesPercentPerCategory payments [(food, 74), (transportation, 27)]
  putStrLn $ show $ givesWarnings overPayments (Just [Warn clothes 100])
  putStrLn $ show $ givesWarnings payments Nothing
