fibonacci_sum :: Integer -> Integer
fibonacci_sum i = sum $ filter (\x -> even x) (takeWhile (\x -> x<i ) fibs)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)