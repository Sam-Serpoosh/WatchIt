module StringUtils where

comma       = ","
arrow       = " -> "
chartPixel  = "#"
colon       = ": "
emptyString = ""
space       = ' '

-- Add trailing space to a string so it'll have desired length
enlarge :: Int -> String -> String
enlarge len str = let remainder = len - (length str)
                  in str ++ (take remainder (repeat space))
