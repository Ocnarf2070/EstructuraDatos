module Bag where

import DataStructures.Set.LinearSet

data Bag a = Empty | Node a Int (Bag a)

empty :: Bag a
empty = empty

isEmpty :: Bag a -> Bool
isEmpty empty = True
isEmpty _     = False

insert :: (Ord a) => a -> Bag a -> Bag a
insert x Empty = Node x 

occurrences :: (Ord a) => a -> Bag a -> Int


delete :: (Ord a) => a -> Bag a -> Bag a