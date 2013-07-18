import Control.Monad
import Data.List

res = sum (minus [1..28123] (sort abundantSums))

abundantSums :: [Int]
abundantSums = specialSum abundants abundants

specialSum :: [Int] -> [Int] -> [Int]
specialSum [] _ = []
specialSum (x:xs) ys = map (x+) (takeWhile (\z -> z <=x) ys) ++ specialSum xs ys

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

abundants :: [Int]
abundants = filter isAbundant [1..28123]

isAbundant :: Int -> Bool
isAbundant i = sum (factors i) > i

factors :: Int -> [Int]
factors = tail.nub.map product.powerset.primeFactors

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

minus (x:xs) (y:ys) = case (compare x y) of
  LT -> x : minus xs (y:ys)
  EQ ->     minus xs    ys
  GT ->     minus (x:xs) ys
minus xs _ = xs