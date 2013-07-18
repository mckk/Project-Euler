import Control.Monad
import Data.List

res = sum.filter isAmicable $ [1..9999]

isAmicable i = i == d (d i) && i /= d i

d :: Int -> Int
d 0 = 0
d i = (sum.nub.map product.powerset.primeFactors $ i) -i 
  
-- Fucking awesome haskell magic
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])
  
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor :: Int -> [Int] -> [Int]
    factor n (p:ps)
      | p*p > n        = [n]
      | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
      | otherwise      = factor n ps