sumOfInts :: Integer
sumOfInts = sum $ filter (\x->x `mod` 5 == 0 || x `mod` 3 == 0) [0..999]