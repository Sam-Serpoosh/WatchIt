module Renderer where

import PaymentTracker

chartPaymentsByCat :: [Payment] -> String
chartPaymentsByCat payments = let presentable = presentableChartForCats . percentPerCategory $ payments
                                  catsChart   = map joinCategoryAndTags presentable
                              in unlines catsChart

joinCategoryAndTags :: (Category, String) -> String
joinCategoryAndTags (cat, tags) = show cat ++ ": " ++ tags

payments :: [Payment]
payments = [
            Payment { value = 10, category = food,           description = "chicken shawerma" },
            Payment { value = 11, category = transportation, description = "uber to work"     },
            Payment { value = 20, category = food,           description = "Noon o Kabab"     },
            Payment { value = 60, category = transportation, description = "uber to airport"  }
           ]

main :: IO ()
main = putStrLn $ chartPaymentsByCat payments
