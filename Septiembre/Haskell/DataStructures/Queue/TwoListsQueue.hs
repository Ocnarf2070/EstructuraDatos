module DataStructures.Queue.TwoListsQueue (Queue, empty, isEmpty, enqueue, dequeue, first) where

import Test.QuickCheck
import Data.List (intersperse)
data Queue a = Q [a] [a]

empty :: Queue a
empty = Q [] []

isEmpty :: Queue a -> Bool
isEmpty (Q [] _) = True
isEmpty _ = False

mkValid :: [a] -> [a] -> Queue a
mkValid [] ys = Q (reverse ys) []
mkValid xs ys = Q xs ys

enqueue :: a -> Queue a -> Queue a
enqueue elem (Q xs ys) = mkValid xs (elem:ys)

first :: Queue a -> a
first (Q [] _) = error "first on empty queue"
first (Q (x:xs) ys) = x

dequeue :: Queue a -> Queue a
dequeue (Q [] _) = error "dequeue on empty queue"
dequeue (Q (x:xs) ys) = mkValid xs ys

instance (Show a) => Show (Queue a) where
	show q@(Q xs ys) = "TwoListsQueue(" ++ concat(intersperse ","(map (show) (queueToList q)))++")"
instance (Eq a) => Eq (Queue a) where
	q1 == q2 = queueToList q1 == queueToList q2
	
queueToList :: Queue a -> [a]
queueToList (Q xs ys) = xs ++ (reverse ys)
		
instance (Arbitrary a) => Arbitrary (Queue a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)