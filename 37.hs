import Data.List

r = sum $ take 11 (filter (\x -> isPTL x && isPTR x && x>9) primes)

isPTL :: Int -> Bool
isPTL = (all isPrime).(map read).(filter (/=[])).tails.show

isPTR :: Int -> Bool
isPTR = (all isPrime).(map (read.reverse)).(filter (/=[])).tails.reverse.show

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