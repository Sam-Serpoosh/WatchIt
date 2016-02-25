module PaymentTracker where

import StringUtils
import Category
import CategoryConfig
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)

type Overpaid = Double
type Percent  = Double
type Month    = String

data Payment = Payment { value       :: Double
                       , category    :: Category
                       , description :: String
                       } deriving (Eq)

instance Show Payment where
  show payment = description payment ++ arrow ++ show (value payment)

-- Adding Payments AGNOSTIC of their Category
instance Monoid Payment where
  mempty = Payment { value       = 0.0
                   , category    = others
                   , description = emptyString 
                   }
  Payment { value = v1 } `mappend` Payment { value = v2 } = Payment { value       = v1 + v2
                                                                    , category    = others
                                                                    , description = emptyString 
                                                                    }

data Warning = Warn { categ    :: Category
                    , overPaid :: Overpaid
                    } deriving (Eq)

instance Show Warning where
  show Warn { categ = cat,  overPaid = over } = show cat ++ arrow ++ show over


-- INPUT : All payments of each month
-- OUTPUT: For each Category the total paid value per Month
-- e.g [(food, [(jan_2016, 100), (feb_2016, 150)]), (clothes, [(jan_2016, 150), (feb_2016, 200)])]
categoryPaidOverMonths :: [(Month, [Payment])] -> [(Category, [(Month, Money)])]
categoryPaidOverMonths monthsPayments =
  let categoriesMonthPaid = map totalPaidInMonthCategories monthsPayments
      allMonthsPaidCats   = foldl (++) [] categoriesMonthPaid
      monthsPaymentsByCat = groupMonthsPaymentsByCat allMonthsPaidCats
  in map factorOutCatMonthsPayments monthsPaymentsByCat

totalPaidInMonthCategories :: (Month, [Payment]) -> [(Category, Month, Money)]
totalPaidInMonthCategories (m, ps) = map (\(cat, paid) -> (cat, m, paid)) $ totalPaidPerCategory ps

groupMonthsPaymentsByCat :: [(Category, Month, Money)] -> [[(Category, Month, Money)]]
groupMonthsPaymentsByCat = groupBy (\(c1, _, _) (c2, _, _) -> c1 == c2) . sortBy (comparing (\(c, _, _) -> c))

factorOutCatMonthsPayments :: [(Category, Month, Money)] -> (Category, [(Month, Money)])
factorOutCatMonthsPayments catMonthsPays =
  let (c, _, _)  = head catMonthsPays
      monthsPays = map (\(_, month, paid) -> (month, paid)) catMonthsPays
  in (c, monthsPays)

-- OUTPUT: What percent of the total money was spent on each Category
-- e.g [(food, 25%), (clothes, 34%)]
-- Percents are approximate so they might NOT add up to 100
percentPerCategory :: [Payment] -> [(Category, Percent)]
percentPerCategory payments =
  let total       = totalPaid payments
      totalPerCat = totalPaidPerCategory payments
  in map (\(cat, paid) -> (cat, calcPercent paid total)) totalPerCat

calcPercent :: Money -> Money -> Percent
calcPercent val total =
  let percent = (ceiling $ val / total * 100) :: Int
  in (fromIntegral percent :: Percent)

-- Group Payments based on their Category
paymentsPerCategory :: [Payment] -> [(Category, [Payment])]
paymentsPerCategory payments =
  let paysByCat = paymentsByCategory payments
  in map (\pays -> (category . head $ pays, pays)) paysByCat

totalPaid :: [Payment] -> Money
totalPaid = value . mconcat

warnings :: [Payment] -> Maybe [Warning]
warnings payments =
  let paidPerCat = totalPaidPerCategory payments
      warns      = generateWarnings paidPerCat
  in if (length warns) == 0 then Nothing else Just warns

generateWarnings :: [(Category, Money)] -> [Warning]
generateWarnings = map catPayToWarning . filter isOverpaid

catPayToWarning :: (Category, Money) -> Warning
catPayToWarning (cat, paid) = Warn { categ = cat, overPaid = (paid - (threshold cat)) }

isOverpaid :: (Category, Money) -> Bool
isOverpaid (cat, paid) = paid > (threshold cat)

totalPaidPerCategory :: [Payment] -> [(Category, Money)]
totalPaidPerCategory payments =
  let paysByCat = paymentsByCategory payments
  in map totalPaidInSameCategory paysByCat

paymentsByCategory :: [Payment] -> [[Payment]]
paymentsByCategory = groupBy (\p1 p2 -> (category p1) == (category p2)) . sortBy (comparing category)

-- INPUT : Payments of the SAME category
totalPaidInSameCategory :: [Payment] -> (Category, Money)
totalPaidInSameCategory payments = (category . head $ payments, totalPaid payments)
