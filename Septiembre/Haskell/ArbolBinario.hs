data TreeB a = EmptyB | NodeB a (TreeB a) (TreeB a) deriving Show
tree1 :: TreeB Int
tree1 = NodeB 1
	(NodeB 2 
		(NodeB 4 EmptyB EmptyB)
		(NodeB 5 EmptyB EmptyB))
	(NodeB 3 
		(NodeB 6 EmptyB EmptyB)
		EmptyB)	

sumB :: (Num a) => TreeB a -> a
sumB EmptyB = 0
sumB (NodeB x i d) = x + sumB i + sumB d

atLevel :: Int -> TreeB a -> [a]
atLevel _ EmptyB = []
atLevel 0 (NodeB x _ _) = [x]
atLevel n (NodeB x i d) = atLevel (n-1) i ++ atLevel (n-1) d

pathsTo :: (Eq a) => a -> TreeB a -> [[a]]
pathsTo x EmptyB = []
pathsTo x (NodeB y i d)
	| x == y = [y] : ps
	| otherwise = ps
	where
	ps = map (y:) (pathsTo x i ++ pathsTo x d)
	
preOrder :: TreeB a -> [a]
preOrder EmptyB = []
preOrder (NodeB x i d) = x : preOrder i ++ preOrder d

inOrder :: TreeB a -> [a]
inOrder EmptyB = []
inOrder (NodeB x i d) = inOrder i ++ x : inOrder d

postOrder :: TreeB a -> [a]
postOrder EmptyB = []
postOrder (NodeB x i d) = postOrder i ++ postOrder d ++ [x]

tree2 :: TreeB Char
tree2 = NodeB '-' (NodeB '+' (NodeB '2' EmptyB EmptyB) (NodeB '*' (NodeB '3' EmptyB EmptyB) (NodeB '5' EmptyB EmptyB))) (NodeB '3' EmptyB EmptyB)