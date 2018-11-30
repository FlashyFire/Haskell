module Fp1 where

circShiftL :: Int -> [a] -> [a]
-- circShiftL 2 [1,2,3,4] == [3,4,1,2]
-- circShiftL (-1) [1,2,3,4] == [4,1,2,3]
circShiftL _ [] = []
circShiftL n xs 
    | (abs n) >= (length xs) = circShiftL (n `mod` (length xs)) xs
    | n == 0 = xs
    | n < 0 = circShiftL (n + (length xs)) xs
    | otherwise = zipWith const (drop n (cycle xs)) xs
