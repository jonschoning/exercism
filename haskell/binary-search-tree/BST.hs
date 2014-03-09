module BST (bstLeft, bstRight, bstValue, singleton, insert, fromList, toList) where

import Data.List (foldl')

data Tree a = Node { bstValue :: a
                  , bstLeft :: Maybe (Tree a)
                  , bstRight :: Maybe (Tree  a) }
             deriving (Show, Eq)

type BST = Tree Int

singleton :: Int -> BST
singleton a = Node a Nothing Nothing

insert :: Int -> BST -> BST
insert x (Node a l r)  | x <= a    = Node a (insertMaybe l) r
                       | otherwise = Node a l (insertMaybe r)
  where insertMaybe = maybe (Just $ singleton x) (Just . (insert x))

fromList :: [Int] -> BST
fromList []     = error "empty tree"
fromList (x:[]) = singleton x
fromList (x:xs) = foldl' (flip insert) (singleton x) xs

toList :: BST -> [Int] 
toList (Node a l r) = maybeTolist l ++ [a] ++ maybeTolist r
  where maybeTolist = maybe [] toList
