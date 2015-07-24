module Category where

type Threshold = Double

data Category = Cat String Threshold
  deriving (Eq, Ord)

instance Show Category where
  show (Cat name _) = name

-- Predefined categories and their threshold which can change

food           = Cat "food" 200
transportation = Cat "transportation" 200
monthlyRoutine = Cat "monthly-routine" 2000
grocery        = Cat "grocery" 300
clothes        = Cat "clothes" 300
others         = Cat "others" 300

categories :: [Category]
categories = [food, transportation, monthlyRoutine, grocery, clothes, others]
