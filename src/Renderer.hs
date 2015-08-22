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
                                            (Just pays) -> unlines $ [formatCatAndItsTotalPaid cat pays] ++ map show pays

formatCatAndItsTotalPaid :: Category -> [Payment] -> String
formatCatAndItsTotalPaid cat payments = (show cat) ++ colon ++ spaceStr ++ (show $ totalPaid payments)

renderWarnings :: Maybe [Warning] -> String
renderWarnings Nothing      = emptyString
renderWarnings (Just warns) = unlines $ map show warns

extractPaymentsOfCat :: [(Category, [Payment])] -> Category -> Maybe [Payment]
extractPaymentsOfCat paysPerCat cat = let paysForCat = filter (\(c, _) -> c == cat) paysPerCat
                                      in if (length paysForCat) == 0 then Nothing else Just (snd . head $ paysForCat)

presentableChartForCats :: [(Category, Percent)] -> [(Category, String)]
presentableChartForCats catPercents = let formattedCats = formatCategories $ map fst catPercents
                                          pixels        = map percentToChartPixels $ map snd catPercents
                                      in zip formattedCats pixels

formatCategories :: [Category] -> [Category]
formatCategories cats = let enlargeFactor = maximum $ map length $ map name cats
                        in map (formatCategory enlargeFactor) cats

formatCategory :: Int -> Category -> Category
formatCategory enlargeFactor cat@(Cat { name = oldname }) = cat { name = enlarge enlargeFactor oldname }

joinCategoryAndTags :: (Category, String) -> String
joinCategoryAndTags (cat, tags) = show cat ++ colon ++ tags

percentToChartPixels :: Percent -> String
percentToChartPixels val = intercalate emptyString $ take (ceiling $ val / 10) (repeat chartPixel)
