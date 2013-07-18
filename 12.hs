import Data.List
res = head $ filter ((>500).nDivisors) tNumbers

nDivisors n = product $ map ((+1) . length) (group $ primeFactors n)

tNumbers :: [Int]
tNumbers = scanl1 (+) [1..]

primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor :: Int -> [Int] -> [Int]
    factor n (p:ps)
      | p*p > n        = [n]
      | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
      | otherwise      = factor n ps