-------------------------------------------------------------------------------
-- Simple client module using a Stack
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module Demos.Stack.StackDemo where

import Stack
import StackAxioms(stackAxioms)


s1 :: Stack Int
s1 = push 3 (push 2 (push 1 empty))

size :: Stack a -> Int
size s
  | isEmpty s =  0
  | otherwise =  1 + size (pop s)

data MiVertice = A|B|C|D|E|F|G deriving (Show,Eq,Enum,Ord)
data Color = Red |  Blue deriving (Eq,Show,Ord)
s2:: Stack (MiVertice,Color)
s2 = push (A,Blue) (push (B,Red) empty)

f stack | color == Blue = show ver
		|otherwise = show "no es azul"
	where 
	(ver,color) = top stack