module Fp2 where

import Prelude hiding (indices)
import Data.Monoid

-- По списку возвращает список пар -- (индекс, элемент)
indices :: [a] -> [(Integer, a)]
indices xs = zip [0..] xs

-- "Обнуляет" элементы данного списка, неудовлетворяющие заданному условию
zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]
zeroBy xs p = map (\x -> if p $ x then x else mempty) xs

{- Формирует список, каждый элемент которого - сумма соответствующих
элементов исходных списков. Длина результата ограничена длиной самого
короткого списка -}
triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]
--triplewiseSum xs ys zs = zipWith (+) xs (zipWith (+) ys zs)
triplewiseSum xs ys zs = zipWith3 (\x y z -> x + y + z) xs ys zs