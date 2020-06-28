module BST
  ( bstLeft
  , bstRight
  , bstValue
  , singleton
  , insert
  , fromList
  , toList
  ) where

import Control.Applicative ((<|>))
import Data.List (foldl')

import Data.Foldable (Foldable, foldMap, toList)
import Data.Monoid ((<>))

data Tree a = Node
  { bstValue :: a
  , bstLeft :: Maybe (Tree a)
  , bstRight :: Maybe (Tree a)
  } deriving (Show, Eq)

type BST = Tree Int

instance Foldable Tree where
  foldMap f (Node x l r) = foldMaybe l <> f x <> foldMaybe r
    where
      foldMaybe = foldMap (foldMap f)

singleton :: Int -> BST
singleton a = Node a Nothing Nothing

insert :: Int -> BST -> BST
insert x (Node a l r)
  | x <= a = Node a (insertMaybe l) r
  | otherwise = Node a l (insertMaybe r)
  where
    insertMaybe t = fmap (insert x) t <|> (Just $ singleton x)

fromList :: [Int] -> BST
fromList [] = error "empty tree"
fromList (x:xs) = foldl' (flip insert) (singleton x) xs
