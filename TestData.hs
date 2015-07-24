module TestData where

import Category
import PaymentTracker

chicken       = Payment { value = 10,  category = food,           description = "chicken shawerma" }
uberToWork    = Payment { value = 11,  category = transportation, description = "uber to work"     }
kebob         = Payment { value = 20,  category = food,           description = "Noon o Kabab"     }
uberToAirport = Payment { value = 60,  category = transportation, description = "uber to airport"  }
priceyClothes = Payment { value = 400, category = clothes,        description = "Zara Fancy"       }

payments = [chicken, uberToWork, kebob]
overPayments = payments ++ [priceyClothes]
