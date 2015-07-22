module Testing where

data TestResult = Pass | Fail String
  deriving (Show)

assertEqual :: (Eq a, Show a) => a -> a -> TestResult
assertEqual expected actual
  | expected == actual = Pass
  | otherwise          = Fail $ "Expected: " ++ show expected ++ " But got: " ++ show actual
