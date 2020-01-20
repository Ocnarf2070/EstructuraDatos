module Cola (
	Cola,
	colaVacia,
	meteEnCola,
	sacaDeCola,
	estaVaciaCola
	) where
type Cola a = [a]
colaVacia :: Cola a
colaVacia = []
estaVaciaCola :: Cola a -> Bool
estaVaciaCola [] = True
estaVaciaCola _ = False
meteEnCola :: Cola a -> a -> Cola a
meteEnCola q x = q ++ [x]
sacaDeCola :: Cola a -> (a, Cola a)
sacaDeCola [] = error "sacaDeCola: cola vacia"
sacaDeCola (x:q) = (x,q)