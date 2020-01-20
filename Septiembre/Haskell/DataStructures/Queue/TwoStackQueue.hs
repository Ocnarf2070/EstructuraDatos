module DataStructures.Queue.TwoStackQueue (Queue, empty, isEmpty, enqueue, dequeue, first) where

import Data.String
import Data.List (intersperse)
import Test.QuickCheck
import qualified DataStructures.Stack.LinearStack as S

data Queue a = TSQ (S.Stack a) (S.Stack a)

empty :: Queue a
empty = TSQ S.empty S.empty

isEmpty :: Queue a -> Bool
isEmpty (TSQ s1 s2) = S.isEmpty s1

mkValid :: S.Stack a -> S.Stack a -> Queue a
mkValid s1 s2 
	| S.isEmpty s1 = TSQ (rev s2) S.empty
	|otherwise = TSQ s1 s2

rev :: S.Stack a -> S.Stack a
rev = rev' S.empty
	where
	rev' s1 s2 
		|S.isEmpty s2 = s1
		|otherwise = rev' (S.push (S.top s2) s1) (S.pop s2)
	
enqueue :: a -> Queue a -> Queue a
enqueue elem (TSQ s1 s2) = mkValid s1 (S.push elem s2)

dequeue :: Queue a -> Queue a
dequeue q@(TSQ s1 s2) 
	|isEmpty q = error "dequeue on a empty queue"
	|otherwise = mkValid (S.pop s1) s2

first :: Queue a -> a  
first q@(TSQ s1 _) =if isEmpty q then error "dequeue on a empty queue" else S.top s1

instance (Eq a , Show a) => Eq (Queue a) where
	q1 == q2 
		|isEmpty q1 && isEmpty q2 = True
		|otherwise = stackToList q1 == stackToList q2
	
instance (Show a) => Show (Queue a) where
	show s = "TwoStackQueue(" ++ concat (intersperse "," (stackToList s))++")"
	
stackToList :: (Show a) => Queue a -> [String]
stackToList q@(TSQ s1 s2) = if isEmpty q then [] else rep s1 ++ rep (rev s2)
	where 
	rep :: (Show a) => S.Stack a -> [String]
	rep s = if S.isEmpty s then [] else show (S.top s) : rep (S.pop s)


instance (Arbitrary a) => Arbitrary (Queue a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)