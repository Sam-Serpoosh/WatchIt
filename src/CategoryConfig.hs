module CategoryConfig where

import Category

-- Predefined categories and their threshold which can change
food, transportation, monthly, grocery, clothes, books, fun, others :: Category

food           = Cat { name = "food",            threshold = 0.00 }
transportation = Cat { name = "transportation",  threshold = 0.00 }
monthly        = Cat { name = "monthly",         threshold = 0.00 }
grocery        = Cat { name = "grocery",         threshold = 0.00 }
clothes        = Cat { name = "clothes",         threshold = 0.00 }
books          = Cat { name = "books",           threshold = 0.00 }
fun            = Cat { name = "fun",             threshold = 0.00 }
others         = Cat { name = "others",          threshold = 0.00 }

validCategories :: [Category]
validCategories = [
                    food,
                    transportation,
                    monthly,
                    grocery,
                    clothes,
                    books,
                    fun,
                    others
                  ]
