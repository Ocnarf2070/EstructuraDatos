module TCola (TCola) where
import Cola

data TCola a = TC [a] deriving Eq

instance Cola TCola where
	estaVaciaCola (TC []) = True
	estaVaciaCola (TC _) = False
	meteEnCola (TC q) x = TC (q++[x])
	sacaDeCola (TC []) = error "sacaDeCola: cola vacia"
	sacaDeCola (TC (x:xs)) = (x,TC xs)
	colaVacia = TC []
	
instance Show a => Show (TCola a) where
	show (TC []) = ""
	show (TC [x]) = show x
	show (TC (x:xs)) = show x ++ "<|" ++ show (TC xs)