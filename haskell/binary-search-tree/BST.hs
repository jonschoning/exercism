module BST (bstLeft, bstRight, bstValue, singleton, insert, fromList, toList) where

import Data.List (foldl')

data Tree a = Tip | Node (Tree  a) a (Tree a)

type BST = Tree Int

bstLeft :: BST -> Maybe BST
bstLeft (Node a _ _) = Just a
bstLeft _ = Nothing

bstRight :: BST -> Maybe BST
bstRight (Node _ _ a) = Just a
bstRight _ = Nothing

bstValue :: BST -> Int
bstValue (Node _ a _) = a
bstValue Tip  = error "no tip value"

singleton :: Int -> BST
singleton a = Node Tip a Tip

insert :: Int -> BST -> BST
insert x (Tip) = error "can't insert tip"
insert x (Node Tip a r) | x <= a = Node (singleton x) a r
insert x (Node l a Tip) | x > a  = Node l a (singleton x)
insert x (Node l a r)   | x <= a = Node (insert x l) a r
insert x (Node l a r)   | x > a  = Node l a (insert x r)

fromList :: [Int] -> BST
fromList [] = Tip
fromList (x:xs) = foldl' (flip insert) (singleton x) xs

toList :: BST -> [Int] 
toList Tip = []
toList (Node Tip a r) = a : toList r
toList (Node l a Tip) = toList l ++ [a]
toList (Node l a r)   = toList l ++ [a] ++ toList r

