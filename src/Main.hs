module Main where

import System.Environment
import System.Directory
import PaymentTracker
import Category
import CategoryConfig
import InputReader
import Renderer
import Data.List (intercalate, isSuffixOf)

delimiter, separator, costs :: String
delimiter = "-------------------------------------------\n"
separator = delimiter ++ delimiter ++ delimiter
costs     = "_costs"

-- Title of Different Sections in the Output
totalPaidTitle, chartTitle, warningsTitle, catDetailsTitle, catsMonthlyChartTitle :: String
totalPaidTitle         = "* TOTAL PAID *"
chartTitle             = "* PAYMENTS CHART *"
warningsTitle          = "!!! WARNINGS !!!"
catDetailsTitle        = "* CATEGORY DETAILS *"
catsMonthlyChartTitle  = "* CATEGORY OVER MONTHS *"

paymentsOutOfFile :: FilePath -> IO [Payment]
paymentsOutOfFile path = do
  content <- readFile path
  return $ contentToPayments content

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

detailsOfCat :: [Payment] -> Category -> String
detailsOfCat payments cat = renderPaymentsOfCategory (paymentsPerCategory payments) cat

drawCatsMonthlySpentChart :: [String] -> [[Payment]] -> IO ()
drawCatsMonthlySpentChart months monthsPayments = do
  putStrLn catsMonthlyChartTitle
  putStrLn $ intercalate delimiter $ barChartCategories $ sortAndLabelMonthsPayments months monthsPayments

-- TWO INPUT ARGS:
-- payment-file-for-detail-report
-- payments-dir-for-bar-charts-of-categories-over-months
main :: IO ()
main = do
  args <- getArgs
  let currFile   = args !! 0
  let paymentDir = args !! 1
  currPayments <- paymentsOutOfFile currFile
  payFiles     <- getDirectoryContents paymentDir
  let months    = filter (isSuffixOf costs) payFiles
  let costFiles = map (paymentDir ++) months
  monthsPayments <- sequence $ map paymentsOutOfFile costFiles
  generateReport currPayments
  putStrLn separator
  drawCatsMonthlySpentChart months monthsPayments
