module Renderer where

import StringUtils
import Category
import PaymentTracker
import Data.Char
import Data.List (intercalate)

chartPaymentsByCat :: [Payment] -> String
chartPaymentsByCat payments =
  let presentable = presentableChartForCats . percentPerCategory $ payments
      catsChart   = map joinCategoryAndBars presentable
  in unlines catsChart

presentableChartForCats :: [(Category, Percent)] -> [(String, String)]
presentableChartForCats catPercents = 
  let formattedCats = alignStrings $ map (show . fst) catPercents
      pixels        = map percentToChartPixels $ map snd catPercents
  in zip formattedCats pixels

joinCategoryAndBars :: (String, String) -> String
joinCategoryAndBars (cat, bars) = cat ++ colon ++ bars

percentToChartPixels :: Percent -> String
percentToChartPixels val = intercalate emptyString $ take (ceiling $ val / 10) (repeat chartPixel)

--
renderPaymentsOfCategory :: [(Category, [Payment])] -> Category -> String
renderPaymentsOfCategory paysPerCat cat =
  let paysOfCat = extractPaymentsOfCat paysPerCat cat
  in case paysOfCat of
    Nothing     -> emptyString
    (Just pays) -> unlines $ [formatCatAndItsTotalPaid cat pays] ++ alignPayments pays

extractPaymentsOfCat :: [(Category, [Payment])] -> Category -> Maybe [Payment]
extractPaymentsOfCat paysPerCat cat =
  let paysForCat = filter (\(c, _) -> c == cat) paysPerCat
  in if (length paysForCat) == 0 then Nothing else Just (snd . head $ paysForCat)

formatCatAndItsTotalPaid :: Category -> [Payment] -> String
formatCatAndItsTotalPaid cat payments = (show cat) ++ colon ++ spaceStr ++ (show $ totalPaid payments)

alignPayments :: [Payment] -> [String]
alignPayments payments =
  let alignedDesc = alignStrings $ map description payments
      values      = map (show . value) payments
  in map (\(desc, val) -> desc ++ arrow ++ val) (zip alignedDesc values)

--
renderWarnings :: Maybe [Warning] -> String
renderWarnings Nothing      = emptyString
renderWarnings (Just warns) = unlines $ alignWarnings warns

alignWarnings :: [Warning] -> [String]
alignWarnings warns =
  let alignedCats = alignStrings $ map (show . categ) warns
      overs       = map (show . overPaid) warns
  in map (\(cat, over) -> cat ++ arrow ++ over) (zip alignedCats overs)

-- Bar Charts for money spent on each category in different months
-- Bar Chart for FOOD, for CLOTHES, etc.
barChartCategories :: [(String, [Payment])] -> [String]
barChartCategories monthsPayments =
  let monthsSpentOnCat = categoryPaidOverMonths monthsPayments
  in map barChartCatSpentMonths monthsSpentOnCat

barChartCatSpentMonths :: (Category, [(String, Money)]) -> String
barChartCatSpentMonths (cat, monthsSpent) =
  let alignedMonths = alignStrings $ map fst monthsSpent
      barPixels     = map valueToPixel $ map snd monthsSpent
      monthPixels   = zip alignedMonths barPixels
      graphBars     = map (\(month, pix) -> month ++ colon ++ pix) monthPixels
  in unlines $ [map toUpper (name cat), emptyString] ++ graphBars

valueToPixel :: Money -> String
valueToPixel money = intercalate emptyString $ take (ceiling $ money / 50) (repeat chartPixel)
