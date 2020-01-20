module DataStructures.PriorityQueue.LinearPriorityQueue (PQueue, empty, isEmpty, enqueue, first, dequeue) where

import Test.QuickCheck
 

data PQueue a = Empty | Node a (PQueue a)

empty :: PQueue a
empty = Empty

isEmpty :: PQueue a -> Bool
isEmpty Empty = True
isEmpty _ = False

enqueue :: (Ord a) => a -> PQueue a -> PQueue a
enqueue x Empty = Node x Empty
enqueue x (Node y q)
	| y <= x = Node y (enqueue x q)
	| otherwise = Node x (Node y q)

first :: PQueue a -> a
first Empty = error "first on empty priority queue"
first (Node a _) = a

dequeue :: PQueue a -> PQueue a
dequeue Empty = error "dequeue on empty priority queue"
dequeue (Node _ q) = q

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