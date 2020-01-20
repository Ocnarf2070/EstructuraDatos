module Pila (module Pila) where

type Pila a = [a]

pilaVacia :: Pila a
pilaVacia = []

estaVaciaPila :: Pila a -> Bool
estaVaciaPila [] = True
estaVaciaPila _ = False

meteEnPila :: Pila a -> a -> Pila a
meteEnPila p x = x : p

sacaDePila :: Pila a -> (a,Pila a)
sacaDePila [] = error "sacaDePila: pila vacia"
sacaDePila (x:xs) = (x,xs)

topeDePila :: Pila a -> a
topeDePila [] = error "topeDePila: pila vacia"
topeDePila (x:_) = x
