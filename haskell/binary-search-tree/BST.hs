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
insert x (Node a Nothing Nothing)  | x <= a = Node a (Just $ singleton x) (Nothing)
                                   | x  > a = Node a (Nothing)            (Just $ singleton x)
insert x (Node a (Just l) Nothing) | x <= a = Node a (Just $ insert x l)  (Nothing)
                                   | x  > a = Node a (Just l)             (Just $ singleton x)
insert x (Node a Nothing (Just r)) | x <= a = Node a (Just $ singleton x) (Just r)
                                   | x  > a = Node a (Nothing)            (Just $ insert x r)
insert x (Node a (Just l) (Just r))| x <= a = Node a (Just $ insert x l)  (Just r)
                                   | x  > a = Node a (Just l)             (Just $ insert x r)


fromList :: [Int] -> BST
fromList []     = error "empty tree"
fromList (x:[]) = singleton x
fromList (x:xs) = foldl' (flip insert) (singleton x) xs

toList :: BST -> [Int] 
toList (Node a l r) = maybeTolist l ++ [a] ++ maybeTolist r
  where maybeTolist = maybe [] toList
