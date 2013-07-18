primes :: [Double]
primes = 2 : filter isPrime [3..]

isPrime :: Double -> Bool
isPrime x = null (factors x)

factors :: Double -> [Double]
factors x = filter (\y -> round x `mod` round y == 0) (takeWhile (\y -> y <= sqrt x) primes)