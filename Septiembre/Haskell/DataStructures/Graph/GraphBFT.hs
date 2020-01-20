module DataStructures.Graph.GraphBFT
  ( bft
  , bftPaths
  ) where
 
import DataStructures.Queue.TwoListsQueue
import qualified DataStructures.Set.BSTSet as S
import DataStructures.Graph.Graph
import qualified DataStructures.Dictionary.BSTDictionary as D
import DataStructures.Graph.Utils

bft :: (Ord a) => Graph a -> a -> [a]
bft g v0 = aux S.empty (enqueue v0 empty)
	where
	aux visited queue
		| isEmpty queue = [] 
		| v `S.isElem` visited = aux visited queue' 
		| otherwise = v : aux visited' (enqueueAll queue' us)
		where
			v = first queue
			queue' = dequeue queue
			visited' = S.insert v visited
			us = [ u | u <- successors g v, u `S.notIsElem` visited ]

data DiEdge a = a :-> a

bftPaths :: (Ord a) => Graph a -> a -> [Path a]
bftPaths g v0 = aux S.empty (enqueue (v0 :-> v0) empty) D.empty
	where
	aux visited queue dict
		| isEmpty queue = [] 
		| v `S.isElem` visited = aux visited queue' dict 
		| otherwise = pathFromTo v0 v dict' : aux visited' (enqueueAll queue' es) dict'
		where
			w :-> v = first queue
			queue' = dequeue queue
			visited' = S.insert v visited
			dict' = D.insert v w dict 
			es = [ v :-> u | u <- successors g v, u `S.notIsElem` visited ]