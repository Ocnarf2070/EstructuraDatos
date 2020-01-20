module DataStructures.Set.SortedLinearSet
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

data Set a = Empty | Node a (Set a)

empty :: Set a
empty = Empty

isEmpty :: Set a -> Bool
isEmpty Empty = True
isEmpty _ = False

size :: Set a -> Int
size Empty = 0
size (Node _ s) = 1 + size s

isElem :: Eq a => a -> Set a -> Bool
isElem elem Empty = False
isElem elem (Node x s)
	|elem == x = True
	|otherwise = isElem elem s 
	
insert ::(Ord a, Eq a) => a -> Set a -> Set a
insert x Empty = Node x Empty
insert x (Node y s)
	| x < y = Node x (Node y s) 
	| x == y = Node y s
	| otherwise = Node y (insert x s)
	
delete :: Eq a => a -> Set a -> Set a
delete elem Empty = Empty
delete elem (Node x s) 
	| elem == x = s
	| otherwise = Node x (delete elem s)
	
fold :: (a -> b -> b) -> b -> Set a -> b
fold f z Empty = z
fold f z (Node x s) = f x (fold f z s)

union :: (Ord a) => Set a -> Set a -> Set a
union s s' = fold insert s s'

difference :: (Ord a) => Set a -> Set a -> Set a
difference s s' = fold delete s s'

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s s'  = fold (\x inter -> if isElem x s then insert x inter else inter) empty s'

instance (Show a) => Show (Set a) where
  show s  = "SortedLinearSet(" ++ intercalate "," (aux s) ++ ")"
    where
      aux Empty       = []
      aux (Node x s)  = show x : aux s

instance (Eq a) => Eq (Set a) where
  s == s'  = s `isSubsetOf` s' && s' `isSubsetOf` s

isSubsetOf :: (Eq a) => Set a -> Set a -> Bool
Empty    `isSubsetOf` s'  = True
Node x s `isSubsetOf` s'  = x `isElem` s' && s `isSubsetOf` s'

instance (Ord a, Eq a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary  = do
      xs <- listOf arbitrary
      return (foldr insert empty xs)