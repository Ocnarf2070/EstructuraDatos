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
   ) where

import Prelude hiding (last)
import Data.List(intercalate)
import Test.QuickCheck

data DEQue a = DEQ [a] [a] --deriving Show

-- Complexity: O(1)
empty :: DEQue a
empty = DEQ [] []

-- Complexity: O(1)
isEmpty :: DEQue a -> Bool
isEmpty (DEQ [] []) = True
isEmpty _ = False

-- Complexity: O(1)
addFirst :: a -> DEQue a -> DEQue a
addFirst x (DEQ fs ls) = DEQ (x:fs) ls

-- Complexity: O(1)
addLast :: a -> DEQue a -> DEQue a
addLast x (DEQ fs ls) = DEQ fs (x:ls)

-- Complexity: O(1) - O(n)
first :: DEQue a -> a
first (DEQ [] ls) = head (reverse ls)
first (DEQ (f:fs) _ ) = f

-- Complexity: O(1) - O(n)
last :: DEQue a -> a
last (DEQ fs []) = head $ reverse fs
last (DEQ _ (l:ls)) = l

-- Complexity: O(1) - O(n)
deleteFirst :: DEQue a -> DEQue a
deleteFirst (DEQ [] []) = error "deleteFirst on empty queue"
deleteFirst (DEQ [] ls) = deleteFirst (DEQ (reverse p2) p1)
	where
	p1 = take (div (length ls) 2) ls
	p2 = drop (div (length ls) 2) ls
deleteFirst (DEQ (f:fs) ls) = DEQ fs ls

-- Complexity: O(1) - O(n)
deleteLast :: DEQue a -> DEQue a
deleteLast (DEQ [] []) = error "deleteLast on empty queue"
deleteLast (DEQ fs []) = deleteLast (DEQ p1 (reverse p2))
	where
	p1 = take (div (length fs) 2) fs
	p2 = drop (div (length fs) 2) fs
deleteLast (DEQ fs (l:ls)) = DEQ fs ls



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
