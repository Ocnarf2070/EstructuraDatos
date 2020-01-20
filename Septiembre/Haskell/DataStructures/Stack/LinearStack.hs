module DataStructures.Stack.LinearStack (
	Stack,
	empty,
	isEmpty,
	push,
	top,
	pop) where
	
import Data.List (intersperse)
import Test.QuickCheck

data Stack a = Empty | Node a (Stack a)

empty :: Stack a
empty = Empty

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False

push :: a -> Stack a -> Stack a
push elem s = Node elem s

pop :: Stack a -> Stack a
pop Empty = error "Empty stack"
pop (Node a s) = s 

top :: Stack a -> a
top Empty = error "Empty stack"
top (Node a s) = a

instance (Eq a) => Eq (Stack a) where
	Empty == Empty = True
	(Node x s) == (Node x' s') = (x==x') && (s==s')
	_ == _ = False
	
instance (Show a) => Show (Stack a) where
	show s = "LinearStack(" ++ concat (intersperse "," (stackToList s))++")"
	
stackToList :: (Show a) => Stack a -> [String]
stackToList Empty = []
stackToList (Node x s) = show x : stackToList s

instance (Arbitrary a) => Arbitrary (Stack a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr push empty xs)

	