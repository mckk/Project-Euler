import Data.List
import Data.Char

res = length $ filter isCircularP (takeWhile (<1000000) primes)

isCircularP :: Int -> Bool
isCircularP = (all isPrime).(map read).rots.show


rots xs = sp (length xs) (length xs) [] (cycle xs)
  where
    sp :: Int -> Int -> [[a]] -> [a] -> [[a]]
    sp 0 _ res _ = res
    sp n l acc (x:xs) = sp (n-1) l ((take l (x:xs)):acc) xs

isPrime 1 = False
isPrime n = (==1) . length . primeFactors $ n

primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor :: Int -> [Int] -> [Int]
    factor n (p:ps)
      | p*p > n        = [n]
      | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
      | otherwise      = factor n ps