module InputReader where

import StringUtils
import Data.List.Split
import PaymentTracker (Payment(..))
import Category (Category(..), Money)
import CategoryConfig

contentToPayments :: String -> [Payment]
contentToPayments content = map lineToPayment (lines content)

lineToPayment :: String -> Payment
lineToPayment line = let (val:cat:desc:_) = splitOn comma line
                         paidValue        = read val :: Money
                         categ            = findCategory cat
                     in Payment { value = paidValue, category = categ, description = desc }

findCategory :: String -> Category
findCategory cat = let definedCat = filter (\c -> name c == cat) validCategories
                   in if (length definedCat == 0) then others else head definedCat
