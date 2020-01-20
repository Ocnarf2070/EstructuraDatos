module DataStructures.PriorityQueue.LinearPriorityQueue2 (PQueue, empty, isEmpty, enqueue, first, dequeue) where

import Test.QuickCheck
 

data PQueue a = Empty | Node a (PQueue a)

empty :: PQueue a
empty = Empty

isEmpty :: PQueue a -> Bool
isEmpty Empty = True
isEmpty _ = False

enqueue :: a -> PQueue a -> PQueue a
enqueue x q = Node x q

dequeue :: (Ord a) => PQueue a -> PQueue a
dequeue Empty = error "dequeue on empty queue"
dequeue q = aux (minimo q) q
	where
	aux x (Node y q) = if x == y then q else Node y (aux x q)

first :: (Ord a) => PQueue a -> a
first Empty = error "first on empty queue"
first q = minimo q	

minimo Empty = error "minimo on empty queue"
minimo (Node x Empty) = x
minimo (Node x q) = min x (minimo q)

instance (Show a) => Show (PQueue a) where
	show q = "LinearPriorityQueue(" ++ queueToString q ++ ")"
		where
		queueToString Empty = ""
		queueToString (Node x Empty) = show x ++ ""
		queueToString (Node x q) = show x ++',': queueToString q
instance (Eq a) => Eq (PQueue a) where
	Empty == Empty = True
	(Node x q) == (Node x' q') = x == x' && q == q'
	_ == _ = False

instance (Ord a, Arbitrary a) => Arbitrary (PQueue a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)

