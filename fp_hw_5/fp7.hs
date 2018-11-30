module Fp7 where

import Data.Foldable
import Data.Monoid

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Foldable Tree where
    foldMap f Nil = mempty
    foldMap f (Node l v r) =
        (foldMap f l) <> f v <> (foldMap f r)

--t = Node Nil 2 (Node (Node Nil 4 Nil) 3 (Node Nil 5 Nil))
--getSum . foldMap (Sum) $ t  

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

instance Foldable Preorder where
    foldMap f (PreO Nil) = mempty
    foldMap f (PreO (Node l v r)) =
        f v <> (foldMap f (PreO l)) <> (foldMap f (PreO r))

instance Foldable Postorder where
    foldMap f (PostO Nil) = mempty
    foldMap f (PostO (Node l v r)) =
        (foldMap f (PostO l)) <> (foldMap f (PostO r)) <> f v

-- See http://aryweb.nl/2013/10/28/haskell-tree-traversal/        
traverseBF :: Tree a -> [a]
traverseBF tree = tbf [tree]
    where
        tbf [] = []
        tbf xs = map nodeValue xs ++ tbf (concat (map subTrees xs))

        nodeValue (Node _ a _) = a

        subTrees (Node Nil _ Nil) = []
        subTrees (Node Nil _ b) = [b]
        subTrees (Node a _ Nil) = [a]
        subTrees (Node a _ b) = [a,b]

instance Foldable Levelorder where
    foldMap f (LevelO tree) = foldMap f (traverseBF tree)
        