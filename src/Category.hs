module Category where

type Threshold = Double
type Money     = Double

data Category = Cat { name      :: String
                    , threshold :: Money
                    } deriving (Eq, Ord)

instance Show Category where
  show Cat { name = catName } = catName
