module Main where

import System.Environment
import PaymentTracker
import Category
import CategoryConfig
import InputReader
import Renderer
import StringUtils (pathDelimiter)
import Data.List.Split
import Data.List (intercalate)

delimiter, separator, detailReport, catOverMonth :: String
delimiter    = "-------------------------------------------\n"
separator    = delimiter ++ delimiter ++ delimiter
detailReport = "detail"
catOverMonth = "cat-chart"

-- Title of Different Sections in the Output
totalPaidTitle, chartTitle, warningsTitle, catDetailsTitle :: String
totalPaidTitle  = "* TOTAL PAID *"
chartTitle      = "* PAYMENTS CHART *"
warningsTitle   = "!!! WARNINGS !!!"
catDetailsTitle = "* CATEGORY DETAILS *"

detailsOfCat :: [Payment] -> Category -> String
detailsOfCat payments cat = renderPaymentsOfCategory (paymentsPerCategory payments) cat

generateReport :: [Payment] -> IO ()
generateReport payments = do
  putStrLn totalPaidTitle
  putStrLn $ show $ totalPaid payments
  putStrLn separator
  putStrLn chartTitle
  putStrLn $ chartPaymentsByCat payments
  putStrLn separator
  putStrLn warningsTitle
  putStrLn $ renderWarnings . warnings $ payments
  putStrLn separator
  putStrLn catDetailsTitle
  putStrLn $ intercalate delimiter $ map (detailsOfCat payments) validCategories

paymentsOutOfFile :: FilePath -> IO [Payment]
paymentsOutOfFile path = do
  content <- readFile path
  return $ contentToPayments content

main :: IO ()
main = do
  args <- getArgs
  let action = args !! 0
  if action == detailReport then do
    let path = args !! 1
    payments <- paymentsOutOfFile path
    generateReport payments
  else do
    let paths  = tail args
    let months = map (last . splitOn pathDelimiter) paths
    monthsPayments <- sequence $ map paymentsOutOfFile paths
    putStrLn $ intercalate delimiter $ barChartCategories $ zip months monthsPayments
