-------------------------------------------------------------------------------
-- Student's name:
-- Student's group:
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle(isEulerian, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g 
	| isEmpty g = False
	| length (vertices g) == 1 = True
	| otherwise = and (map (\xs -> length xs >= 2 && mod (length xs) 2 == 0) (map (successors g) (vertices g)))

-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u) =  foldl deleteVertex g' a' 
	where 	
		g' = deleteEdge g (v,u)
		a' = [v|v <- vertices g' , degree g' v ==0]

-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = aux g v0 [v0]
	where aux g v xs
		|u==head xs = (remove g (v,u) , xs ++ [u])
		|otherwise = aux (remove g  (v,u)) u (xs++[u])
			where u = head (successors g v)

-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles xs (y:ys) 
	| length xs == 0 = (y:ys)
	| head xs == y = (y:ys) ++ tail xs
	| otherwise = head xs : connectCycles (tail xs) (y:ys)

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g cycle 
	| elem (head cycle) (vertices g) == True = head cycle
	| otherwise = vertexInCommon g (tail cycle)

-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g 
	| not $ isEulerian g = error "No es euleriano"
	| otherwise = aux g []
		where 
		aux g' [] = aux f (connectCycles [] s)
			where
			(f,s) = extractCycle g' (head $ vertices g')
		aux g' path
			| isEmpty g' = path 
			| otherwise = aux f (connectCycles path s)
			where
			(f,s) = extractCycle g' (vertexInCommon g' path)