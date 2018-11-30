module Fp5 where
import Prelude hiding (Monoid,mappend)

class Monoid m where
    mappend :: m -> m -> m
    mempty :: m

--merge function
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | (x <= y)  = x:(merge xs (y:ys)) 
    | otherwise = y:(merge (x:xs) ys)

newtype SortedList a = SortedList { getSorted :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (SortedList a) where
    mempty = SortedList []
    mappend l r = SortedList $ merge (getSorted l) (getSorted r)
