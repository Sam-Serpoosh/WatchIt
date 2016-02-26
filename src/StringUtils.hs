module StringUtils where

import qualified Data.Map as DM
import System.Time
import Data.List.Split

comma, arrow, chartPixel, colon, emptyString, spaceStr, pathDelimiter :: String
comma          = ","
arrow          = " -> "
chartPixel     = "#"
colon          = ": "
emptyString    = ""
spaceStr       = " "
monthYearDelim = "_"
pathDelimiter  = "/"

space :: Char
space = ' '

namesToMonths :: DM.Map String Month
namesToMonths = DM.fromList [
                              ("jan", January), ("feb", February), ("mar", March),
                              ("apr", April),   ("may", May),      ("jun", June),
                              ("jul", July),    ("aug", August),   ("sep", September),
                              ("oct", October), ("nov", November), ("dec", December)
                            ]

getMonthByName :: String -> Month
getMonthByName monName =
  let month = DM.lookup monName namesToMonths
  in case month of
    Nothing    -> January
    (Just mon) -> mon

-- Only care about Month & Year in this context so rest of values are dummy values
createCalTime :: Month -> Int -> CalendarTime
createCalTime month year = CalendarTime year month 1 0 0 0 0 Sunday 0 "UTC" 0 False

-- INPUT : String in the form of mon_year_costs (e.g jan_2016_costs)
-- OUTPUT: Tuple of Month & Year
extractMonthAndYear :: String -> (Month, Int)
extractMonthAndYear monthYear =
  let (mon:year:_) = splitOn monthYearDelim monthYear
  in (getMonthByName mon, read year :: Int)

monthYearToLabel :: (Month, Int) -> String
monthYearToLabel (month, year) = show month ++ monthYearDelim ++ show year

alignStrings :: [String] -> [String]
alignStrings strs =
  let enlargeFactor = maximum $ map length strs
  in map (enlarge enlargeFactor) strs

-- Add trailing space to a string so it'll have desired length
enlarge :: Int -> String -> String
enlarge len str =
  let remainder = len - (length str)
  in str ++ (take remainder (repeat space))
