res = length . concatMap name $ [1..1000]

name :: Int -> String
name i = if (i==1000) then "onethousand" else parseH i
parseH i = case i `div` 100 of
  0 -> parseD False i
  a -> (parseS False a) ++ "hundred" ++ (parseD True (i-100*a))

parseD an i = (case i `div` 10 of
  0 -> parseS an i
  a -> (if an then "and" else "") ++ case a of
    1 -> parseST i
    2 -> "twenty" ++ (parseS False (i - a*10))
    3 -> "thirty" ++ (parseS False (i - a*10))
    4 -> "forty" ++ (parseS False (i - a*10))
    5 -> "fifty" ++ (parseS False (i - a*10))
    6 -> "sixty" ++ (parseS False (i - a*10))
    7 -> "seventy" ++ (parseS False (i - a*10))
    8 -> "eighty" ++ (parseS False (i - a*10))
    9 -> "ninety" ++ (parseS False (i - a*10)))

parseS an i = case i of
  0 -> ""
  1 -> (if an then "and" else "") ++ "one"
  2 -> (if an then "and" else "") ++ "two"
  3 -> (if an then "and" else "") ++ "three"
  4 -> (if an then "and" else "") ++ "four"
  5 -> (if an then "and" else "") ++ "five"
  6 -> (if an then "and" else "") ++ "six"
  7 -> (if an then "and" else "") ++ "seven"
  8 -> (if an then "and" else "") ++ "eight"
  9 -> (if an then "and" else "") ++ "nine"

parseST i = case i of
  10 -> "ten"
  11 -> "eleven"
  12 -> "twelve"
  13 -> "thirteen"
  14 -> "fourteen"
  15 -> "fifteen"
  16 -> "sixteen"
  17 -> "seventeen"
  18 -> "eighteen"
  19 -> "nineteen"