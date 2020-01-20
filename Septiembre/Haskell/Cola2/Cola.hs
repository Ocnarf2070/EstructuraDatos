module Cola(Cola(..)) where
class Cola q where

	colaVacia :: q a
	estaVaciaCola :: q a -> Bool
	meteEnCola :: q a -> a -> q a
	sacaDeCola :: q a -> (a,q a)