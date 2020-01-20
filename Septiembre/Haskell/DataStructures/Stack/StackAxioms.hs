module DataStructures.Stack.StackAxioms(ax1,ax2,ax3,ax4,stackAxioms) where

import DataStructures.Stack.StackOnList --LinearStack
import Test.QuickCheck

-- top debe devolver el último elemento apilado
ax1 x s =  True ==>  top (push x s) == x

-- Hacer pop tras push no modifica el stack
ax2 x s =  True ==>  pop (push x s) == s

-- un stack vacío está vacío
ax3     =  True ==>  isEmpty empty

-- push devuelve un stack no vacío
ax4 x s =  True ==>  not (isEmpty (push x s))

type Elem = Int
	 
stackAxioms = do
  quickCheck (ax1 :: Elem -> Stack Elem -> Property)
  quickCheck (ax2 :: Elem -> Stack Elem -> Property)
  quickCheck (ax3 :: Property)
  quickCheck (ax4 :: Elem -> Stack Elem -> Property)
