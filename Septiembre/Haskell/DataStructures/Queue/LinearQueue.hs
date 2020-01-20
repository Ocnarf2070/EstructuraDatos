module DataStructures.Queue.LinearQueue (Queue, empty, isEmpty, enqueue, dequeue, first) where

import Test.QuickCheck
data Queue a = Empty | Node a (Queue a)

empty :: Queue a
empty = Empty

isEmpty :: Queue a -> Bool
isEmpty Empty = True
isEmpty _ = False

enqueue :: a -> Queue a -> Queue a
enqueue x Empty = Node x Empty
enqueue x (Node y q) = Node y (enqueue x q)

first :: Queue a -> a
first Empty = error "first on empty queue"
first (Node x q) = x

dequeue :: Queue a -> Queue a
dequeue Empty = error "dequeue on empty queue"
dequeue (Node x q) = q

instance (Show a) => Show (Queue a) where
	show q = "LinearQueue(" ++ queueToString q ++ ")"
		where
		queueToString Empty = ""
		queueToString (Node x Empty) = show x ++ ""
		queueToString (Node x q) = show x ++',': queueToString q
instance (Eq a) => Eq (Queue a) where
	Empty == Empty = True
	(Node x q) == (Node x' q') = x == x' && q == q'
	_ == _ = False

instance (Arbitrary a) => Arbitrary (Queue a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)