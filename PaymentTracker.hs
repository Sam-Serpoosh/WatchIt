module PaymentTracker where

import Data.List (groupBy, intercalate)

type Threshold = Double

data Category = Cat String Threshold
  deriving (Show, Eq)

data Payment = Payment { value       :: Double
                       , title       :: String
                       , category    :: Category
                       , description :: String
                       } deriving (Show)

chartPixel  = "#"
emptyString = ""

-- Predefine categories and their threshold which can change
food           = Cat "food" 200
transportation = Cat "transportation" 200
monthlyRoutine = Cat "monthly-routine" 2000
grocery        = Cat "grocery" 300
clothes        = Cat "clothes" 300
others         = Cat "others" 300

categories :: [Category]
categories = [food, transportation, monthlyRoutine, grocery, clothes, others]

-- Keep in mind the percents are approximate so
-- if they won't add up to 100% it's OK!
percentPerCategory :: [Payment] -> [(Category, Double)]
percentPerCategory payments = let total = totalPayment payments
                                  totalPerCat = totalPerCategory payments
                              in map (\(cat, val) -> (cat, calcPercent val total)) totalPerCat

presentableChartForCats :: [(Category, Double)] -> [(Category, String)]
presentableChartForCats = map (\(cat, percent) -> (cat, percentToHashTags percent))

totalPayment :: [Payment] -> Double
totalPayment payments = sum $ map (\p -> value p) payments

totalPerCategory :: [Payment] -> [(Category, Double)]
totalPerCategory payments = let groupedByCat = groupBy (\p1 p2 -> (category p1) == (category p2)) payments
                                totalPerCat  = map (\pays -> (category . head $ pays, sum $ map value pays)) groupedByCat
                            in totalPerCat

calcPercent :: Double -> Double -> Double
calcPercent val total = let percent = floor $ val / total * 100
                        in (fromIntegral percent :: Double)

percentToHashTags :: Double -> String
percentToHashTags val = intercalate emptyString $ take (floor $ val / 10) (repeat chartPixel)
