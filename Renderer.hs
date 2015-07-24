module Renderer where

import Category
import PaymentTracker
import Data.List (intercalate)

type Percent = Double

chartPixel  = "#"
colon       = ": "
emptyString = ""

chartPaymentsByCat :: [Payment] -> String
chartPaymentsByCat payments = let presentable = presentableChartForCats . percentPerCategory $ payments
                                  catsChart   = map joinCategoryAndTags presentable
                              in unlines catsChart

renderPaymentsOfCategory :: [(Category, [Payment])] -> Category -> String
renderPaymentsOfCategory paysPerCat cat = let paysOfCat = extractPaymentsOfCat paysPerCat cat
                                          in case paysOfCat of
                                            Nothing     -> emptyString
                                            (Just pays) -> unlines $ [show cat] ++ map show pays

renderWarnings :: Maybe [Warning] -> String
renderWarnings Nothing      = emptyString
renderWarnings (Just warns) = unlines $ map show warns

extractPaymentsOfCat :: [(Category, [Payment])] -> Category -> Maybe [Payment]
extractPaymentsOfCat paysPerCat cat = let paysForCat = filter (\(c, pays) -> c == cat) paysPerCat
                                      in if (length paysForCat) == 0 then Nothing else Just (snd . head $ paysForCat)

presentableChartForCats :: [(Category, Percent)] -> [(Category, String)]
presentableChartForCats = map (\(cat, percent) -> (cat, percentToHashTags percent))

joinCategoryAndTags :: (Category, String) -> String
joinCategoryAndTags (cat, tags) = show cat ++ colon ++ tags

percentToHashTags :: Percent -> String
percentToHashTags val = intercalate emptyString $ take (ceiling $ val / 10) (repeat chartPixel)
