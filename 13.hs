main = do
  nums <- fmap ((map read).lines) (readFile "13.dat")
  print . take 10 . show . sum $ nums