import List

findPalindrome :: Integer
findPalindrome = last $ filter isPalindrome (sort [i*j | i<-[999,998..0], j <-[i,i-1..0]])

isPalindrome :: Integer -> Bool
isPalindrome s = isPalindromeS (show s)
  where
    isPalindromeS :: String -> Bool
    isPalindromeS [] = True
    isPalindromeS (s:[])  = True
    isPalindromeS s@(x:xs) = (head s == last s) && (isPalindromeS $ tail (init s))