module Renderer where

import StringUtils
import Category
import PaymentTracker
import Data.List (intercalate)

chartPaymentsByCat :: [Payment] -> String
chartPaymentsByCat payments = let presentable = presentableChartForCats . percentPerCategory $ payments
                                  catsChart   = map joinCategoryAndTags presentable
                              in unlines catsChart

renderPaymentsOfCategory :: [(Category, [Payment])] -> Category -> String
renderPaymentsOfCategory paysPerCat cat = let paysOfCat = extractPaymentsOfCat paysPerCat cat
                                          in case paysOfCat of
                                            Nothing     -> emptyString
                                            (Just pays) -> unlines $ [formatCatAndItsTotalPaid cat pays] ++ map show (alignPayments pays)

renderWarnings :: Maybe [Warning] -> String
renderWarnings Nothing      = emptyString
renderWarnings (Just warns) = unlines $ map show (alignWarnings warns)

presentableChartForCats :: [(Category, Percent)] -> [(Category, String)]
presentableChartForCats catPercents = let formattedCats = alignCategories $ map fst catPercents
                                          pixels        = map percentToChartPixels $ map snd catPercents
                                      in zip formattedCats pixels

joinCategoryAndTags :: (Category, String) -> String
joinCategoryAndTags (cat, tags) = show cat ++ colon ++ tags

extractPaymentsOfCat :: [(Category, [Payment])] -> Category -> Maybe [Payment]
extractPaymentsOfCat paysPerCat cat = let paysForCat = filter (\(c, _) -> c == cat) paysPerCat
                                      in if (length paysForCat) == 0 then Nothing else Just (snd . head $ paysForCat)

formatCatAndItsTotalPaid :: Category -> [Payment] -> String
formatCatAndItsTotalPaid cat payments = (show cat) ++ colon ++ spaceStr ++ (show $ totalPaid payments)

alignPayments :: [Payment] -> [Payment]
alignPayments payments = let enlargeFactor = maximum $ map (length . description) payments
                         in map (alignPayment enlargeFactor) payments

alignCategories :: [Category] -> [Category]
alignCategories cats = let enlargeFactor = maximum $ map (length . name) cats
                       in map (alignCategory enlargeFactor) cats

percentToChartPixels :: Percent -> String
percentToChartPixels val = intercalate emptyString $ take (ceiling $ val / 10) (repeat chartPixel)

alignPayment :: Int -> Payment -> Payment
alignPayment enlargeFactor payment@(Payment { description = oldDesc }) = payment { description = enlarge enlargeFactor oldDesc }

alignCategory :: Int -> Category -> Category
alignCategory enlargeFactor cat@(Cat { name = oldName }) = cat { name = enlarge enlargeFactor oldName }

alignWarnings :: [Warning] -> [Warning]
alignWarnings warns = let alignedCats  = alignCategories $ map categ warns
                          zipped       = zip warns alignedCats
                      in map (\(warn, alignedCat) -> warn { categ = alignedCat }) zipped
