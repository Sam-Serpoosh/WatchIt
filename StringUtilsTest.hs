module StringUtilsTest where

import Testing
import StringUtils

-- Tests for enlargeByLongest

enlargesStringsByLongestElement :: Int -> String -> String -> TestResult
enlargesStringsByLongestElement enlargeFactor str expectedStr = assertEqual expectedStr (enlarge enlargeFactor str)

main :: IO ()
main = do
  putStrLn $ show $ enlargesStringsByLongestElement 5 "fo" "fo   "
