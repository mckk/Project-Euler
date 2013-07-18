import Data.List

res = (sort (perms "0123456789")) !! 999999

perms [] = [[]]
perms xs = [x:ys | x<-xs, ys <- perms (delete x xs)]