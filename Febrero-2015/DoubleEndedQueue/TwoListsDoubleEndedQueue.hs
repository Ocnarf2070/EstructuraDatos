-------------------------------------------------------------------------------
-- Estructuras de Datos. Grado en Informática, IS e IC. UMA.
-- Examen de Febrero 2015.
--
-- Implementación del TAD Deque
--
-- Apellidos:
-- Nombre:
-- Grado en Ingeniería ...
-- Grupo:
-- Número de PC:
-------------------------------------------------------------------------------

module TwoListsDoubleEndedQueue
   ( DEQue
   , empty
   , isEmpty
   , first
   , last
   , addFirst
   , addLast
   , deleteFirst
   , deleteLast
   ,deleteLastRec
   ) where

import Prelude hiding (last)
import Data.List(intercalate)
import Test.QuickCheck

data DEQue a = DEQ [a] [a]

-- Complexity:
empty :: DEQue a
empty =  DEQ [] []

-- Complexity:
isEmpty :: DEQue a -> Bool
isEmpty (DEQ [] []) =  True
isEmpty _     =  False
-- Complexity:
addFirst :: a -> DEQue a -> DEQue a
addFirst e (DEQ xs ys) = DEQ (e:xs) (ys)

-- Complexity:
addLast :: a -> DEQue a -> DEQue a
addLast e (DEQ xs ys) = DEQ (xs) (e:ys)

-- Complexity:
first :: DEQue a -> a
first (DEQ [] ys) = first (DEQ (reverse p1) p2)
	where
		p1=take ((length ys)`div`2) ys
		p2=drop ((length ys)`div`2) ys
		reverse xs = revOn xs []
			where	
				revOn [] ys=ys
				revOn (x:xs) ys = revOn xs (x:ys)
first (DEQ (x:xs) ys) = x

-- Complexity:
last :: DEQue a -> a
last (DEQ xs []) = last (DEQ p1 (reverse p2))
	where
		p1=take ((length xs)`div`2) xs
		p2=drop ((length xs)`div`2) xs
		reverse xs = revOn xs []
			where	
				revOn [] ys=ys
				revOn (x:xs) ys = revOn xs (x:ys)
last (DEQ xs (y:ys)) = y

-- Complexity:
deleteFirst :: DEQue a -> DEQue a
deleteFirst (DEQ [] ys) = deleteFirst (DEQ (reverse p1) p2)
	where
		p1=take ((length ys)`div`2) ys
		p2=drop ((length ys)`div`2) ys
		reverse xs = revOn xs []
			where	
				revOn [] ys=ys
				revOn (x:xs) ys = revOn xs (x:ys)
deleteFirst (DEQ (x:xs) ys) = DEQ xs ys

-- Complexity:
deleteLast :: DEQue a -> DEQue a
deleteLast (DEQ xs []) = deleteLast (DEQ p1 (reverse p2))
	where
		p1=take ((length xs)`div`2) xs
		p2=drop ((length xs)`div`2) xs
		reverse xs = revOn xs []
			where	
				revOn [] ys=ys
				revOn (x:xs) ys = revOn xs (x:ys)
deleteLast (DEQ xs (y:ys)) = DEQ xs ys

deleteLastRec (DEQ (xs) (ys)) n
	 | n>0 = deleteLastRec (deleteLast (DEQ (xs) (ys))) (n-1)
	 |otherwise = (DEQ (xs) (ys))
instance (Show a) => Show (DEQue a) where
   show q = "TwoListsDoubleEndedQueue(" ++ intercalate "," [show x | x <- toList q] ++ ")"

toList :: DEQue a -> [a]
toList (DEQ xs ys) =  xs ++ reverse ys

instance (Eq a) => Eq (DEQue a) where
   q == q' =  toList q == toList q'

instance (Arbitrary a) => Arbitrary (DEQue a) where
   arbitrary =  do
      xs <- listOf arbitrary
      ops <- listOf (oneof [return addFirst, return addLast])
      return (foldr id empty (zipWith ($) ops xs))
