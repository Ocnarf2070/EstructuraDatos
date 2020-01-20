module DataStructures.Graph.Graph
 ( Graph
 , Edge
 , Path
 , mkGraphSuc
 , mkGraphEdges
 , successors
 , vertices
 , edges
 , degree
 ) where
 
import Data.List

data Graph a = G [a] (a -> [a])

mkGraphSuc :: [a] -> (a -> [a]) -> Graph a
mkGraphSuc vs suc = G vs suc

type Edge a = (a,a)

mkGraphEdges :: Eq a => [a] -> [Edge a] -> Graph a
mkGraphEdges vs es = G vs suc
	where
	suc v = nub ([ y | (x,y) <- es, x ==v] ++ [ x | (x,y) <- es, y==v])
	
type Path a = [a]

successors :: Graph a -> a -> [a]
successors (G vs suc) v = suc v

vertices :: Graph a -> [a]
vertices (G vs suc) = vs

edges :: (Eq a) => Graph a -> [Edge a]
edges (G vs sucs)  = [ (v,w) | v <- vs, w <- sucs v ]

degree :: Graph a -> a -> Int
degree g v = length (successors g v)

instance (Eq a, Show a) => Show (Graph a) where
  show g@(G vs sucs)  = "Graph("++vertices++","++arcs++")"
   where
    vertices  = "("++ intercalate "," (map show vs) ++")"
    arcs  = "(" ++ intercalate ", " (map showEd $ nubBy cmp (edges g)) ++ ")"
    cmp (x,y) (x',y')  = (x==x' && y==y') || (x==y' && y==x')
    showEd (x,y)  = show x++" - "++show y
	
g1 :: Graph Int
g1 = mkGraphSuc [1,2,3,4] suc
	where
	suc 1 = [2,3]
	suc 2 = [1,3]
	suc 3 = [1,2,4]
	suc 4 = [3]	
	
g1' :: Graph Int
g1' = mkGraphEdges [1,2,3,4] [(1,2), (1,3), (2,3), (3,4)]