module StringUtils where

comma, arrow, chartPixel, colon, emptyString, spaceStr :: String
comma       = ","
arrow       = " -> "
chartPixel  = "#"
colon       = ": "
emptyString = ""
spaceStr    = " "

space :: Char
space = ' '

alignStrings :: [String] -> [String]
alignStrings strs = let enlargeFactor = maximum $ map length strs
                    in map (enlarge enlargeFactor) strs

-- Add trailing space to a string so it'll have desired length
enlarge :: Int -> String -> String
enlarge len str = let remainder = len - (length str)
                  in str ++ (take remainder (repeat space))
