module DataStructures.Queue.QueueAxioms (ax1,ax2,ax3,ax4,ax5,ax6,queueCheckAxioms) where

--import DataStructures.Queue.LinearQueue
import DataStructures.Queue.TwoStackQueue 
import Test.QuickCheck

ax1 x = first (enqueue x empty) == x
ax2 x q = not (isEmpty q) ==> first (enqueue x q) == first q
ax3 x q = not (isEmpty q) ==> dequeue (enqueue x q) == enqueue x (dequeue q)
ax4 x = dequeue (enqueue x empty) == empty
ax5 = isEmpty empty
ax6 x q = not (isEmpty (enqueue x q))


type Elem = Int -- Tipo a usar en las pruebas

queueCheckAxioms = do
	quickCheck (ax1 :: Elem -> Bool)
	quickCheck (ax2 :: Elem -> Queue Elem -> Property)
	quickCheck (ax3 :: Elem -> Queue Elem -> Property)
	quickCheck (ax4 :: Elem -> Bool)
	quickCheck (ax5 :: Bool)
	quickCheck (ax6 :: Elem -> Queue Elem -> Bool)
