module Fp6 where
import Prelude hiding(Monoid,mappend)
import Fp5

fsthalf :: [a] -> [a]
fsthalf xs = take (length xs `div` 2) xs

sndhalf :: [a] -> [a]
sndhalf xs = drop (length xs `div` 2) xs

msort :: Ord a => [a] -> SortedList a
msort [] = SortedList []
msort [x] = SortedList [x]
msort xs = (msort (fsthalf xs)) `mappend` (msort (sndhalf xs))
