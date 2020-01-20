module DataStructures.PriorityQueue.WBLeftistHeapPriorityQueue
  ( PQueue
  , empty
  , isEmpty
  , first
  , dequeue
  , enqueue
  ) where

import qualified DataStructures.Heap.WBLeftistHeap as H
import Data.List(intercalate)
import Test.QuickCheck

data PQueue a = PQ (H.Heap a)

empty :: PQueue a 
empty = PQ H.empty

isEmpty :: PQueue a -> Bool
isEmpty (PQ h) = H.isEmpty h

first :: PQueue a -> a
first (PQ h) = H.minElem h

enqueue :: (Ord a) => a -> PQueue a -> PQueue a 
enqueue x (PQ h) = PQ (H.insert x h)

dequeue :: (Ord a) => PQueue a -> PQueue a
dequeue (PQ h) = PQ (H.delMin h)

heapToList :: (Ord a) => H.Heap a -> [a]
heapToList h
 | H.isEmpty h = []
 | otherwise   = H.minElem h : heapToList (H.delMin h)
 
instance (Ord a, Show a) => Show (PQueue a) where
  show (PQ h)  = "WBLeftistHeapPriorityQueue(" ++ intercalate "," (map show . heapToList $ h) ++ ")"

instance (Ord a) => Eq (PQueue a) where
  (PQ h) == (PQ h')  = heapToList h == heapToList h'

instance (Ord a, Arbitrary a) => Arbitrary (PQueue a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)