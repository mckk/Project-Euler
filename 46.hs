r = head $ filter (notGoldbach) (([3,5..] `minus` [5777,5993]) `minus` (primes))

notGoldbach :: Integer -> Bool
notGoldbach n = notElem n [x+2*y^2 | x <- (takeWhile (<n) primes), y <- [1..squareRoot ((n-x) `div` 2)]]

primes :: [Integer]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]


minus (x:xs) (y:ys) = case (compare x y) of
  LT -> x : minus xs (y:ys)
  EQ ->     minus xs    ys
  GT ->     minus (x:xs) ys
minus xs _ = xs

primeFactors n = factor n primes
  where
    factor :: Integer -> [Integer] -> [Integer]
    factor n (p:ps)
      | p*p > n        = [n]
      | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
      | otherwise      = factor n ps
      
squareRoot :: Integer -> Integer
squareRoot = floor . sqrt . (fromIntegral :: Integer -> Double)
