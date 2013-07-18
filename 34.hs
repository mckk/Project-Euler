import Data.Char

res = sum $ filter (\x -> x == special x) [3..100000]

special = sum . map (factorial.digitToInt) . show

factorial n = product [1..n]