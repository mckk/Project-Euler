import Data.List
res = length $ nub [a^b | a <- [2..100], b <- [2..100]]