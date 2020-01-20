module DataStructures.Heap.WBLeftistHeap
  ( Heap
  , empty
  , isEmpty
  , minElem
  , delMin
  , insert
  ) where
  
import DataStructures.Graphics.DrawTrees
import Test.QuickCheck

  
data Heap a = Empty | Node a Int (Heap a) (Heap a)

weight :: Heap a -> Int
weight Empty           = 0
weight (Node _ w _ _)  = w

size :: Heap a -> Int
size = weight

empty :: Heap a 
empty = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _ = False

minElem :: Heap a -> a
minElem Empty = error "minElem on empty heap"
minElem (Node x _ _ _) = x

delMin :: (Ord a) => Heap a -> Heap a
delMin Empty = error "delMin on empty heap"
delMin (Node _ _ lh rh) = merge lh rh

singleton :: a -> Heap a
singleton x = Node x 1 Empty Empty

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (singleton x) h 

node :: a -> Heap a -> Heap a -> Heap a
node x h h'
 | w >= w'    = Node x s h h'
 | otherwise  = Node x s h' h
 where
   w = weight h
   w' = weight h'
   s = w + w' + 1

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty h'     = h'
merge h     Empty  = h
merge h@(Node x w lh rh) h'@(Node x' w' lh' rh')
 | x <= x'         = node x lh (merge rh h') 
 | otherwise       = node x' lh' (merge h rh')
	
mkHeap :: (Ord a) => [a] -> Heap a
mkHeap []  = empty
mkHeap xs  = mergeLoop (map singleton xs)
  where
    -- mergeLoop []  = empty
    mergeLoop [h] = h
    mergeLoop hs  = mergeLoop (mergePairs hs)

    mergePairs []         = []
    mergePairs [h]        = [h]
    mergePairs (h:h':hs)  = merge h h' : mergePairs hs
	
	
instance Subtrees (Heap a) where
  subtrees Empty             = []
  subtrees (Node x w lh rh)  = [lh,rh]

  isEmptyTree  = isEmpty

instance (Show a) => ShowNode (Heap a) where
  showNode (Node x _ _ _)  = show x

drawOnWith :: FilePath -> (a -> String) -> Heap a -> IO ()
drawOnWith file toString  = _drawOnWith file showHeap
 where
  showHeap (Node x _ _ _)  = toString x
  
instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = do
    kvs <- arbitrary
    return (mkHeap kvs)

	