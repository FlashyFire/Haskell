module Fp3 where

import Data.Char
import Data.List

revRange :: (Char,Char) -> [Char]
--revRange r = unfoldr (\c -> if c == pred (ord (fst r)) then Nothing else Just((chr c), pred c)) (ord (snd r))
revRange = unfoldr fun

fun (first, last)
    | first > last = Nothing
    | last == '\NUL' = Just (last, (succ last, last))
    | otherwise = Just (last, (first, pred last))