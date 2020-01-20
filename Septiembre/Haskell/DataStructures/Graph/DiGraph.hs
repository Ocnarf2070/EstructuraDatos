module DataStructures.Graph.DiGraph 
 ( DiGraph
 , DiEdge((:->))
 , Path
 , mkDiGraphSuc
 , mkDiGraphEdges
 , successors
 , predecesors
 , vertices
 , diEdges
 , outDegree
 , inDegree
-- , deleteVertices
 )
 where
 
data DiGraph a = DG [a] (a -> [a])

mkDiGraphSuc :: [a] -> (a -> [a]) -> DiGraph a
mkDiGraphSuc vs suc = DG vs suc

data DiEdge a = a :-> a deriving Show

mkDiGraphEdges :: (Eq a) => [a] -> [DiEdge a] -> DiGraph a
mkDiGraphEdges vs es = DG vs suc
	where
	suc v = [ y | x :-> y <- es, x==v ]
	
successors :: DiGraph a -> a -> [a]
successors (DG vs suc) v = suc v

predecesors :: (Eq a) => DiGraph a -> a -> [a]
predecesors (DG vs suc) v = [ w | w <- vs, v `elem` suc w ]

vertices :: DiGraph a -> [a]
vertices (DG vs suc) = vs

outDegree :: DiGraph a -> a -> Int
outDegree g v = length (successors g v)

inDegree :: (Eq a) => DiGraph a -> a -> Int
inDegree g v = length (predecesors g v)

diEdges :: DiGraph a -> [DiEdge a]
diEdges (DG vs suc)  = [ v :-> w | v <- vs, w <- suc v ]

instance (Eq a, Show a) => Show (DiGraph a) where
  show g@(DG vs sucs)  = "DiGraph("++verts++","++arcs++")"
   where
    verts = "("++ intercalate "," (map show vs) ++")"
    arcs  = "(" ++ intercalate ", " (map show (diEdges g)) ++ ")"