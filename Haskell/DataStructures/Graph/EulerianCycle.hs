-------------------------------------------------------------------------------
-- Student's name:
-- Student's group:
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle(isEulerian,extractCycle,f, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List


data Vertex = A | B | C | D | E | F | G | H | I | J deriving (Show,Eq,Enum,Ord)

g3 :: Graph Vertex -- eulerian
g3 = mkGraphEdges [A .. C]
                  [(A,B), (B,C), (C,A)]


--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g 
	| isEmpty g = False
	| length(vertices g) == 1 = True
	| otherwise = (filter (\x->length x `mod` 2 == 1) (map (successors g) (vertices g))) == []
-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u) = foldl deleteVertex g' a' 
	where 	
		g' = deleteEdge g (v,u)
		a' = [v|v <- vertices g' , degree g' v ==0]


-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = aux g v0 [v0]
	where aux g v (x:vs)
		|u==x = (remove g (v,u) , x:vs ++ [u])
		|otherwise = aux (remove g  (v,u)) u (x:vs++[u])
			where u = head (successors g v)

-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles xs (y:ys) | length xs == 0 = (y:ys)
						| (head xs) == y = (y:ys) ++ (tail xs)
						| otherwise = (head xs) : (connectCycles (tail xs) (y:ys))

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g cycle = head[v | v <- vertices g, elem v cycle]
--
f g v = vertexInCommon g' cycle
	where (g',cycle) = extractCycle g v
-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g 
	| not(isEulerian g)=error "Grafo no euleriano"
	| otherwise = aux g (head (vertices g)) []
		where 
			aux g v es
				|isEmpty g' = es'
				|otherwise = aux g' v' es'
					where 
						(g',cycle) = extractCycle g v
						v'=vertexInCommon g' cycle
						es'=connectCycles es cycle