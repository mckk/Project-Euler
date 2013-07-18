p3 :: Int -> Int
p3 n = n*(n+1) `div` 2

p4 :: Int -> Int
p4 n = n*n

p5 :: Int -> Int
p5 n = n*(3*n-1) `div` 2

p6 :: Int -> Int
p6 n = n*(2*n-1)

p7 :: Int -> Int
p7 n = n*(5*n-3) `div` 2

p8 :: Int -> Int
p8 n = n*(3*n-2)

listOfLength :: (Int -> Int) -> Int -> [Int]
listOfLength f len = takeWhile (\x -> length(show x) == len) $ dropWhile (\x -> length(show x) < len) (map f [1..])

getAllListsOfLength :: Int -> [[Int]]
getAllListsOfLength l = [getL p3, getL p4, getL p5, getL p6, getL p7, getL p8]
  where
    getL = (flip listOfLength) l
    