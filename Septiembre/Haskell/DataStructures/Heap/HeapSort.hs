module DataStructures.Heap.HeapSort(heapSort) where

import DataStructures.Heap.WBLeftistHeap
import Data.List
import Test.QuickCheck

heapSort :: (Ord a) => [a] -> [a]
heapSort = heapToList . mkHeap

heapToList :: (Ord a) => Heap a -> [a]
heapToList h
 | isEmpty h  = []
 | otherwise = minElem h : heapToList (delMin h)


isPermutationOf :: (Eq a) => [a] -> [a] -> Bool
xs `isPermutationOf` ys = null (xs \\ ys) && null (ys \\ xs)

sorted :: (Ord a) => [a] -> Bool
sorted []           = True
sorted [_]          = True
sorted (x:xs@(y:_)) = (x<=y) && sorted xs

p_heapSort xs =  True ==> sorted ys && ys `isPermutationOf` xs
                   where ys = heapSort xs
