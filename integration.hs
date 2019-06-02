integration :: (Double->Double)->Double->Double->Double
integration f a b = 
    let s = (b-a) / 1000
    in intstep f a s 0

intstep :: (Double->Double)->Double->Double->Double->Double
intstep f a s n
    | n < 1000 = 
        let x = a + s * n
        in (f x + f (x + s)) * s / 2 + intstep f a s (n + 1)
    | otherwise = 0