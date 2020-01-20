module DataStructures.Dictionary.AVLDictionary (Dict, empty, isEmpty, insert, delete, get, keys, values, keysValues, mapValues, valueOf) where
import qualified DataStructures.SearchTree.AVL as T
import Data.List(intercalate)
import Data.Char
data Rel a b = a :-> b deriving Show
data Dict a b = D (T.AVL (Rel a b)) --deriving Show

empty :: Dict a b
empty = D T.empty

isEmpty :: Dict a b -> Bool
isEmpty (D avl) = T.isEmpty avl

insert :: Ord a => a -> b -> Dict a b -> Dict a b
insert k v (D avl) = D (T.insert (k :-> v) avl)

delete :: Ord a => a -> Dict a b -> Dict a b
delete x (D avl) = D (T.delete (x :-> undefined) avl)

get :: Ord a => a -> Dict a b -> Maybe b
get x (D avl) = case T.search (x :-> undefined) avl of
	Nothing -> Nothing
	Just (k :-> v) -> Just v

keys :: Dict a b -> [a]
keys (D avl) = map (\x@(k :-> v) -> k) (T.inOrder avl)

values :: Dict a b -> [b]
values (D avl) = map (\x@(k :-> v) -> v) (T.inOrder avl)

keysValues :: Dict a b -> [(a,b)]
keysValues dict = zip (keys dict) (values dict)

mapValues :: Ord a => (b -> c) -> Dict a b -> Dict a c
mapValues f dict = foldr (\y@(c,d) -> insert c d) empty $ map (\x@(a,b) -> (a,f b)) (keysValues dict)

valueOf :: Ord a => a -> Dict a b -> Maybe b
valueOf k (D avl) = case T.search (k :-> undefined) avl of 
	Nothing -> Nothing
	Just (_ :-> v) -> Just v

instance (Eq a) => Eq (Rel a b) where
	(k :-> _) == (k' :-> _) = (k == k')
	
instance (Ord a) => Ord (Rel a b) where
	(k :-> _) <= (k' :-> _) = (k <= k')
	
instance (Show a, Show b) => Show (Dict a b) where
  show (D avl)  = "AVLDictionary(" ++ intercalate "," (aux (T.inOrder avl)) ++ ")"
   where
    aux []             = []
    aux (x:->y : xys)  = (show x++"->"++show y) : aux xys
	
t1 = foldr f empty [0..10]
	where
	f x dict = if mod x 2 == 0 then insert x 'a' dict else insert x 'b' dict