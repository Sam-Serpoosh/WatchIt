module Main where

import System.Environment
import PaymentTracker
import Category
import CategoryConfig
import InputReader
import Renderer
import Data.List (intercalate)

delimiter, separator:: String
delimiter = "-------------------------------------------\n"
separator = delimiter ++ delimiter ++ delimiter

-- Title of Different Sections in the Output
totalPaidTitle, chartTitle, warningsTitle, catDetailsTitle :: String
totalPaidTitle  = "* TOTAL PAID *"
chartTitle      = "* PAYMENTS CHART *"
warningsTitle   = "!!! WARNINGS !!!"
catDetailsTitle = "* CATEGORY DETAILS *"

detailsOfCat :: [Payment] -> Category -> String
detailsOfCat payments cat = renderPaymentsOfCategory (paymentsPerCategory payments) cat

showWarnings :: [Payment] -> String
showWarnings = renderWarnings . warnings

generateReport :: [Payment] -> IO ()
generateReport payments = do
  putStrLn totalPaidTitle
  putStrLn $ show $ totalPaid payments
  putStrLn separator
  putStrLn chartTitle
  putStrLn $ chartPaymentsByCat payments
  putStrLn separator
  putStrLn warningsTitle
  putStrLn $ showWarnings  payments
  putStrLn separator
  let catDetails = map (detailsOfCat payments) validCategories
  putStrLn catDetailsTitle
  putStrLn $ intercalate delimiter catDetails

main :: IO ()
main = do
  args    <- getArgs
  content <- readFile (args !! 0) -- First arg is file-name
  let payments = contentToPayments content
  generateReport payments
