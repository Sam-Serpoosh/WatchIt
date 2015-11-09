module PaymentTracker where

import StringUtils
import Category
import CategoryConfig
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)


data Payment = Payment { value       :: Double
                       , category    :: Category
                       , description :: String
                       } deriving (Eq)

instance Show Payment where
  show payment = description payment ++ arrow ++ show (value payment)

-- For the ability of adding TWO payments together agnostic of category!
instance Monoid Payment where
  mempty = Payment { value = 0.0, category = others, description = emptyString }
  Payment { value = v1 } `mappend` Payment { value = v2 } = Payment { value = v1 + v2, category = others, description = emptyString }

type Overpaid = Double

data Warning = Warn { categ    :: Category
                    , overPaid :: Overpaid
                    } deriving (Eq)

instance Show Warning where
  show Warn { categ = cat,  overPaid = over } = show cat ++ arrow ++ show over

type Percent = Double

-- Keep in mind the percents are approximate so they might not add up to 100
percentPerCategory :: [Payment] -> [(Category, Percent)]
percentPerCategory payments = let total       = totalPaid payments
                                  totalPerCat = totalPaidPerCategory payments
                              in map (\(cat, paid) -> (cat, calcPercent paid total)) totalPerCat

paymentsPerCategory :: [Payment] -> [(Category, [Payment])]
paymentsPerCategory payments = let paysByCat = paymentsByCategory payments
                               in map (\pays -> (category . head $ pays, pays)) paysByCat

totalPaid :: [Payment] -> Money
totalPaid = value . mconcat

warnings :: [Payment] -> Maybe [Warning]
warnings payments = let paidPerCat = totalPaidPerCategory payments
                        warns      = generateWarnings paidPerCat
                    in if (length warns) == 0 then Nothing else Just warns

generateWarnings :: [(Category, Money)] -> [Warning]
generateWarnings = map catPayToWarning . filter isOverpaid

catPayToWarning :: (Category, Money) -> Warning
catPayToWarning (cat, paid) = Warn { categ = cat, overPaid = (paid - (threshold cat)) }

isOverpaid :: (Category, Money) -> Bool
isOverpaid (cat, paid) = paid > (threshold cat)

totalPaidPerCategory :: [Payment] -> [(Category, Money)]
totalPaidPerCategory payments = let paysByCat = paymentsByCategory payments
                                in map totalPaidInSameCategory paysByCat

paymentsByCategory :: [Payment] -> [[Payment]]
paymentsByCategory = groupBy (\p1 p2 -> (category p1) == (category p2)) . sortBy (comparing category)

totalPaidInSameCategory :: [Payment] -> (Category, Money)
totalPaidInSameCategory payments = (category . head $ payments, totalPaid payments)

calcPercent :: Money -> Money -> Percent
calcPercent val total = let percent = (ceiling $ val / total * 100) :: Int
                        in (fromIntegral percent :: Percent)
