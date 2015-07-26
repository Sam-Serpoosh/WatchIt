module InputReader where

import System.Environment
import Data.List.Split
import PaymentTracker (Payment(..))
import Category (Category, categories, others)

comma = ","

contentToPayments :: String -> [Payment]
contentToPayments content = map lineToPayment (lines content)

lineToPayment :: String -> Payment
lineToPayment line = let (val:cat:desc:rest) = splitOn comma line
                         paidValue           = read val :: Money
                         categ               = findCategory cat
                     in Payment { value = paidValue, category = categ, description = desc }

findCategory :: String -> Category
findCategory cat = let definedCat = filter (\(Cat name _) -> name == cat) categories
                   in if (length definedCat == 0) then others else head definedCat
