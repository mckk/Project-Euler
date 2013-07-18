import Data.Char
res = sum.take 6 . filter (\x -> x == power5 x) $ [2..]

power5 :: Int -> Int
power5 n = sum $ (map ((^5).digitToInt)) (show n)