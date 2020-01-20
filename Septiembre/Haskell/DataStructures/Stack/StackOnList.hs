module DataStructures.Stack.StackOnList (
	Stack,
	empty,
	isEmpty,
	push,
	top,
	pop) where

import Data.List(intercalate)
import Test.QuickCheck

data Stack a  = SonL [a]

empty :: Stack a
empty = SonL []

isEmpty :: Stack a -> Bool
isEmpty (SonL []) = True
isEmpty _ = False

push :: a -> Stack a -> Stack a
push elem (SonL ls) = SonL (elem:ls)

top :: Stack a -> a
top (SonL []) = error "Empty stack"
top (SonL (l:ls))= l

pop :: Stack a -> Stack a
pop (SonL []) = error "Empty stack"
pop (SonL (l:ls))= SonL ls

instance (Show a) => Show (Stack a) where
  show (SonL xs)  = "StackOnList(" ++ intercalate "," (map show xs) ++ ")"

instance (Eq a) => Eq (Stack a) where
  (SonL xs) == (SonL xs')  = xs==xs'  

instance (Arbitrary a) => Arbitrary (Stack a) where
    arbitrary  = do
      xs <- listOf arbitrary
      return (foldr push empty xs)