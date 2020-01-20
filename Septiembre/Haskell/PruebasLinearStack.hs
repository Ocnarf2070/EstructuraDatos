module DataStructures.Stack.StackAxioms (ax1,ax2,ax3,ax4,stackCheckAxioms) where
import DataStructures.Stack.LinearStack
import Test.QuickCheck

-- top debe devolver el último elemento apilado
ax1 x s = top (push x s) == x

-- Hacer pop tras push no modifica el stack
ax2 x s = pop (push x s) == s

-- un stack vacío está vacío
ax3 = isEmpty empty

-- push devuelve un stack no vacío
ax4 x s = not (isEmpty (push x s))

type Elem = Int

stackCheckAxioms = do
	quickCheck (ax1 :: Elem -> Stack Elem -> Bool)
	quickCheck (ax2 :: Elem -> Stack Elem -> Bool)
	quickCheck (ax3 :: Bool)
	quickCheck (ax4 :: Elem -> Stack Elem -> Bool)