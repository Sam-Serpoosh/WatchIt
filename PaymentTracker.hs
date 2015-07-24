module PaymentTracker where

import Category
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)

arrow      = " -> "

data Payment = Payment { value       :: Double
                       , category    :: Category
                       , description :: String
                       } deriving (Eq)

instance Show Payment where
  show payment = description payment ++ arrow ++ show (value payment)

type Overpaid = Double

data Warning = Warn Category Overpaid
  deriving (Eq)

instance Show Warning where
  show (Warn cat over) = show cat ++ arrow ++ show over

-- Keep in mind the percents are approximate so
-- if they won't add up to 100% it's OK!
percentPerCategory :: [Payment] -> [(Category, Double)]
percentPerCategory payments = let total       = totalPaid payments
                                  totalPerCat = totalPaidPerCategory payments
                              in map (\(cat, paid) -> (cat, calcPercent paid total)) totalPerCat

paymentsPerCategory :: [Payment] -> [(Category, [Payment])]
paymentsPerCategory payments = let paysByCat = paymentsByCategory payments
                               in map (\pays -> (category . head $ pays, pays)) paysByCat

totalPaid :: [Payment] -> Double
totalPaid payments = sum $ map value payments

warnings :: [Payment] -> Maybe [Warning]
warnings payments = let paidPerCat = totalPaidPerCategory payments
                        warnings   = generateWarnings paidPerCat
                    in if (length warnings) == 0 then Nothing else Just (warnings)

generateWarnings :: [(Category, Double)] -> [Warning]
generateWarnings = map catPayToWarning . filter isOverpaid

catPayToWarning :: (Category, Double) -> Warning
catPayToWarning (cat@(Cat name threshold), paid) = Warn cat (paid - threshold)

isOverpaid :: (Category, Double) -> Bool
isOverpaid ((Cat name threshold), paid) =  paid > threshold

totalPaidPerCategory :: [Payment] -> [(Category, Double)]
totalPaidPerCategory payments = let paysByCat = paymentsByCategory payments
                                in map (\pays -> (category . head $ pays, sum $ map value pays)) paysByCat

paymentsByCategory :: [Payment] -> [[Payment]]
paymentsByCategory = groupBy (\p1 p2 -> (category p1) == (category p2)) . sortBy (comparing category)

calcPercent :: Double -> Double -> Double
calcPercent val total = let percent = ceiling $ val / total * 100
                        in (fromIntegral percent :: Double)
