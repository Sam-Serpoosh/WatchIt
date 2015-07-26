module Main where

import System.Environment
import PaymentTracker
import Category
import InputReader
import Renderer

percentChart    = "chart"
categoryDetails = "cat-details"
warns           = "warnings"
knownActions    = "Actions: chart -- cat-details -- warnings"

act :: String -> [String] -> [Payment] -> String
act action args payments
  | action == percentChart    = drawChart payments
  | action == categoryDetails = detailsOfCat payments (findCategory $ args !! 2) -- Desired Category
  | action == warns           = showWarnings payments
  | otherwise                 = knownActions

totalSpent :: [Payment] -> Money
totalSpent = totalPaid

drawChart :: [Payment] -> String
drawChart = chartPaymentsByCat

detailsOfCat :: [Payment] -> Category -> String
detailsOfCat payments cat = renderPaymentsOfCategory (paymentsPerCategory payments) cat

showWarnings :: [Payment] -> String
showWarnings = renderWarnings . warnings

main :: IO ()
main = do
  args    <- getArgs
  content <- readFile (args !! 0) -- First arg is file-name
  let payments = contentToPayments content
  let action   = args !! 1
  putStrLn $ act action args payments
