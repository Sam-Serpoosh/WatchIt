module Category where

type Threshold = Double
type Money     = Double

data Category = Cat { name      :: String
                    , threshold :: Money
                    } deriving (Eq, Ord)

instance Show Category where
  show Cat { name = catName } = catName

-- Predefined categories and their threshold which can change
food, transportation, monthly, grocery, clothes, books, fun, others :: Category

food           = Cat { name = "food",            threshold = 200  }
transportation = Cat { name = "transportation",  threshold = 200  }
monthly        = Cat { name = "monthly",         threshold = 2000 }
grocery        = Cat { name = "grocery",         threshold = 300  }
clothes        = Cat { name = "clothes",         threshold = 300  }
books          = Cat { name = "books",           threshold = 100  }
fun            = Cat { name = "fun",             threshold = 100  }
others         = Cat { name = "others",          threshold = 300  }

validCategories :: [Category]
validCategories = [food, transportation, monthly, grocery, clothes, books, fun, others]
