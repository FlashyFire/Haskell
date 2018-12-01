module Fp5 where
    
import Data.Semigroup
import Data.Monoid

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

instance Ord a => Semigroup (SortedList a) where
    (<>) = mappend
