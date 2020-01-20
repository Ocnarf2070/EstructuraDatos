module BCola (BCola) where
import Cola

data BCola a = BC [a] [a]

instance Cola BCola where
	colaVacia = BC [] []
	estaVaciaCola (BC [] r) = True
	estaVaciaCola (BC _ r) = False
	meteEnCola (BC f r) x =check f (x:r)
	sacaDeCola (BC [] _) = error "sacaDeCola: cola vacia"
	sacaDeCola (BC (x:f) r) = (x,check f r)
	
inv :: [a] -> [a]
inv = flip inv2 []
inv2 [] ys = ys
inv2 (x:xs) ys = inv2 xs (x:ys)

check :: [a] -> [a] -> BCola a
check [] r = BC (inv r) []
check f r = BC f r

flush :: [a] -> [a] -> BCola a
flush f r = BC (f ++ (inv r)) []

instance Show a => Show(BCola a) where
	show (BC q r) = show' f'
		where
		show' [] = ""
		show' [x] = show x
		show' (x:xs) = show x ++ "<|" ++ show' (xs)
		(BC f' []) = flush q r
		
instance Eq a => Eq (BCola a) where
	(BC q r) == (BC q' r') = f == f'
		where
			BC f [] = flush q r
			BC f' [] = flush q' r'