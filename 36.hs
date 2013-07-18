import Numeric
import Data.Char

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs
  | head xs == last xs = isPalindrome.tail.init $ xs
  | otherwise    = False
  
palindromes10 :: [Int]
palindromes10 = filter (isPalindrome.show) [0..]

palindromes210 :: [Int]
palindromes210 = filter (\x -> isPalindrome $ showIntAtBase 2 intToDigit x "") palindromes10

res :: Int
res = sum $ takeWhile (<1000000) palindromes210