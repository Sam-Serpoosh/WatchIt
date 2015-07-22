module PaymentTracker where

import Data.List (sortBy, groupBy, intercalate)
import Data.Ord (comparing)

type Threshold = Double

data Category = Cat String Threshold
  deriving (Eq, Ord)

instance Show Category where
  show (Cat name _) = name

data Payment = Payment { value       :: Double
                       , category    :: Category
                       , description :: String
                       } deriving (Eq)

instance Show Payment where
  show payment = description payment ++ " -> " ++ show (value payment) ++ " -> " ++ show (category payment)

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
                                  totalPerCat = totalPaysPerCategory payments
                              in map (\(cat, val) -> (cat, calcPercent val total)) totalPerCat

presentableChartForCats :: [(Category, Double)] -> [(Category, String)]
presentableChartForCats = map (\(cat, percent) -> (cat, percentToHashTags percent))

totalPayment :: [Payment] -> Double
totalPayment payments = sum $ map (\p -> value p) payments

paymentsPerCategory :: [Payment] -> [(Category, [Payment])]
paymentsPerCategory payments = let paysByCat = paymentsByCategory payments
                               in map (\pays -> (category . head $ pays, pays)) paysByCat

totalPaysPerCategory :: [Payment] -> [(Category, Double)]
totalPaysPerCategory payments = let groupedByCat = paymentsByCategory payments
                                    totalPerCat  = map (\pays -> (category . head $ pays, sum $ map value pays)) groupedByCat
                                in totalPerCat

paymentsByCategory :: [Payment] -> [[Payment]]
paymentsByCategory = groupBy (\p1 p2 -> (category p1) == (category p2)) . sortBy (comparing category)

calcPercent :: Double -> Double -> Double
calcPercent val total = let percent = floor $ val / total * 100
                        in (fromIntegral percent :: Double)

percentToHashTags :: Double -> String
percentToHashTags val = intercalate emptyString $ take (floor $ val / 10) (repeat chartPixel)
