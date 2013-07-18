import Data.List
import Data.List.Split
import Data.Char

main = do
  names <- readFile "names.txt"
  print . getValue. sort . map (filter (/= '\"')) . (splitOn ",") $ names


getValue = sum.map eval.zip [1..]
  where
    eval (n, st) = n * (getValS st)

getValS = sum.map getValC 
getValC c = ord c - ord 'A' + 1