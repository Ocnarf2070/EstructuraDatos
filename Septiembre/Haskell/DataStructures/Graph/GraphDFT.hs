module DataStructures.Graph.GraphDFT
  ( dft
  , dftPaths
  ) where
  
import DataStructures.Stack.LinearStack
import qualified DataStructures.Set.BSTSet as S
import qualified DataStructures.Dictionary.BSTDictionary as D
import DataStructures.Graph.Utils
import DataStructures.Graph.Graph

g1 :: Graph Int
g1 = mkGraphSuc [1,2,3,4] suc
	where
	suc 1 = [2,3]
	suc 2 = [1,3]
	suc 3 = [1,2,4]
	suc 4 = [3]	
	
g1' :: Graph Int
g1' = mkGraphEdges [1,2,3,4] [(1,2), (1,3), (2,3), (3,4)]

-- pushAll :: Stack a -> [a] -> Stack a
-- pushAll s xs = foldr push s xs

dft :: (Ord a) => Graph a -> a -> [a]
dft g v0 = aux S.empty (push v0 empty)
	where
	aux visited stack
		| isEmpty stack = [] 
		| v `S.isElem` visited = aux visited stack' 
		| otherwise = v : aux visited' (pushAll stack' us)
		where
		v = top stack
		stack' = pop stack
		visited' = S.insert v visited
		us = [ u | u <- successors g v, u `S.notIsElem` visited ]
		
data AnEdge a = a :-> a 

dftPaths :: (Ord a) => Graph a -> a -> [Path a]
dftPaths g v0 = aux S.empty (push (v0 :-> v0) empty) D.empty
	where
		aux visited stack dict
			| isEmpty stack = []
			| v `S.isElem` visited = aux visited stack' dict 
			| otherwise = (pathFromTo v0 v dict') : (aux visited' (pushAll stack' es) dict')
			where
				w :-> v = top stack
				stack' = pop stack
				visited' = S.insert v visited
				dict' = D.insert v w dict 
				es = [ v :-> u | u <- successors g v, u `S.notIsElem` visited ]

