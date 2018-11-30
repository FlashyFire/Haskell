module Fp4 where

-- tying the knot
f x k = y:ys where 
    y  = x / k
    ys = f y k

seriesK :: Int -> [Rational]
-- take 3 $ seriesK 2 == [1 % 1,1 % 2,1 % 4]
-- take 4 $ seriesK 3 == [1 % 1,1 % 3,1 % 9,1 % 27]
seriesK k = (toRational 1) : (f (toRational 1) (toRational k))
