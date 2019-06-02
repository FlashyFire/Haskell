fibp :: Integer -> (Integer, Integer)
fibp 1 = (0, 1)
fibp n = let (n1, n2) = fibp (n - 1)
    in (n2, (n1 + n2))

fibn :: Integer -> (Integer, Integer)
fibn (-1) = (0, 1)
fibn n = let (n1, n2) = fibn (n + 1)
    in (n2, (n1 - n2))

fibonacci :: Integer -> Integer
fibonacci n 
    | n > 0 = snd (fibp n)
    | n < 0 = snd (fibn n)
    | otherwise = 0
