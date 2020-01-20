module DataStructures.Set.ListSet
 ( Set
 , empty
 , isEmpty
 , size
 , insert
 , isElem
 , delete
 
 , fold
 , union
 , intersection
 , difference
 ) where
 
import Data.List(intercalate)
import Test.QuickCheck

import Data.List(intercalate)
import Test.QuickCheck

data Set a = St [a]

empty :: Set a
empty = St []

isEmpty :: Set a -> Bool
isEmpty (St []) = True
isEmpty _ = False

size :: Set a -> Int
size (St []) = 0
size (St (x:xs)) = 1 + size (St xs)

isElem :: Eq a => a -> Set a -> Bool
isElem x (St xs) = elem x xs
	
insert :: Eq a => a -> Set a -> Set a
insert elem (St []) = St [elem]
insert x s@(St xs)  = if elem x xs then s else St (x:xs)
	
delete :: Eq a => a -> Set a -> Set a
delete elem (St []) = St []
delete elem (St s) = St (elim elem s)
	where 
	elim x [] = []
	elim x (y:ys) = if x /= y then y : elim x ys else ys
		
	
fold :: (a -> b -> b) -> b -> Set a -> b
fold f z  (St xs) = foldr f z xs

union :: (Ord a) => Set a -> Set a -> Set a
union s s' = fold insert s s'

difference :: (Ord a) => Set a -> Set a -> Set a
difference s s' = fold delete s s'

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s s'  = fold (\x inter -> if isElem x s then insert x inter else inter) empty s'

instance (Show a) => Show (Set a) where
  show (St xs)  = "ListSet(" ++ Data.List.intercalate "," (map show xs) ++ ")"

instance (Eq a) => Eq (Set a) where
  s == s'  = s `isSubsetOf` s' && s' `isSubsetOf` s

isSubsetOf :: (Eq a) => Set a -> Set a -> Bool
(St xs) `isSubsetOf` (St ys) = isSub xs ys
 where
   []     `isSub` _   = True
   (x:xs) `isSub` ys  = x `elem` ys && xs `isSub` ys

instance (Eq a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary  = do
      xs <- listOf arbitrary
      return (foldr insert empty xs)