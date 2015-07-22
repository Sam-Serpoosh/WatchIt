module PaymentTracker where

data Category = Cat String Double
  deriving (Show)

data Payment = Payment { value       :: Double
                       , title       :: String
                       , category    :: Category
                       , description :: String
                       } deriving (Show)

-- Predefine categories and their threshold which can change
food           = Cat "food" 200
transportation = Cat "transportation" 200
monthlyRoutine = Cat "monthly-routine" 2000
grocery        = Cat "grocery" 300
clothes        = Cat "clothes" 300
others         = Cat "others" 300

categories :: [Category]
categories = [food, transportation, monthlyRoutine, grocery, clothes, others]

totalPayment :: [Payment] -> Double
totalPayment payments = sum $ map (\p -> value p) payments


