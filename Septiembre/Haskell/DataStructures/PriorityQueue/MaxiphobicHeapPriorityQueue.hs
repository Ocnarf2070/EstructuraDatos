module DataStructures.PriorityQueue.MaxiphobicHeapPriorityQueue
  ( PQueue
  , empty
  , isEmpty
  , first
  , dequeue
  , enqueue
  ) where

import qualified DataStructures.Heap.MaxiphobicHeap as M
import Data.List(intercalate)
import Test.QuickCheck

data PQueue a = PQ (M.Heap a)

empty :: PQueue a 
empty = PQ M.empty

isEmpty :: PQueue a -> Bool
isEmpty (PQ h) = M.isEmpty h

first :: PQueue a -> a
first (PQ h) = M.minElem h

enqueue :: (Ord a) => a -> PQueue a -> PQueue a 
enqueue x (PQ h) = PQ (M.insert x h)

dequeue :: (Ord a) => PQueue a -> PQueue a
dequeue (PQ h) = PQ (M.delMin h)

heapToList :: (Ord a) => M.Heap a -> [a]
heapToList h
 | M.isEmpty h = []
 | otherwise   = M.minElem h : heapToList (M.delMin h)
 
instance (Ord a, Show a) => Show (PQueue a) where
  show (PQ h)  = "MaxiphobicHeapPriorityQueue(" ++ intercalate "," (map show . heapToList $ h) ++ ")"

instance (Ord a) => Eq (PQueue a) where
  (PQ h) == (PQ h')  = heapToList h == heapToList h'

instance (Ord a, Arbitrary a) => Arbitrary (PQueue a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)