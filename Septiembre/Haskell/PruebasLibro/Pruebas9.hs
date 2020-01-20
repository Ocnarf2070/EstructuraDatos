data Arbol a = Vacio | Nodo a [Arbol a] deriving Show

a1 :: Arbol Integer
a1 = Nodo 10 [a11,a12,a13]
	where
	a11 = Nodo 22 [Nodo 15 [],Nodo 12[]]
	a12 = Nodo 35 []
	a13 = Nodo 52 [Nodo 33[]]
	
raiz :: Arbol a -> a
raiz Vacio = error "raiz de arbol vacio"
raiz (Nodo x _) = x

tamano :: Arbol a -> Integer
tamano Vacio = 0
tamano (Nodo _ xs) = 1 + sum (map tamano xs)

profundidad :: Arbol a -> Integer
profundidad Vacio = 0
profundidad (Nodo _ []) = 1
profundidad (Nodo _ xs) = 1 + maximum (map profundidad xs)

esHoja :: Arbol a -> Bool
esHoja (Nodo _ []) = True
esHoja _ = False

sumArbol :: Arbol Integer -> Integer
sumArbol Vacio = 0
sumArbol (Nodo n xs) = n + sum (map sumArbol xs)

maxArbol :: (Ord a,Eq a) => Arbol a -> a
maxArbol Vacio = error "Arbol vacio"
maxArbol (Nodo n [] ) = n
maxArbol (Nodo n xs)= max n (maximum(map maxArbol xs))

instance Functor Arbol where
	--fmap f Vacio = Vacio
	--fmap f (Nodo x xs) = Nodo (f x) [fmap f x | x <- xs]
	fmap f = foldArbol (\r ss -> Nodo (f r) ss) Vacio
	
foldArbol :: (a -> [b] -> b) -> b -> Arbol a -> b
foldArbol f e Vacio = e
foldArbol f e (Nodo x xs) = f x (map (foldArbol f e) xs)

tamano' :: Arbol a -> Integer
tamano' = foldArbol (\r ts -> 1+sum ts) 0

sumArbol' :: Num a => Arbol a -> a 
sumArbol' = foldArbol (\r ss -> r + sum ss) 0

todosArbol :: (a -> Bool) -> Arbol a -> Bool
todosArbol p Vacio = True
todosArbol p (Nodo x xs) = p x && and (map (todosArbol p) xs)

todosArbol' :: (a -> Bool) -> Arbol a -> Bool
todosArbol' p = foldArbol (\r bs -> p r && and bs) True

algunoArbol :: (a -> Bool) -> Arbol a -> Bool
algunoArbol p Vacio = True
algunoArbol p (Nodo x xs) = p x || or (map (algunoArbol p) xs)

algunoArbol' :: (a -> Bool) -> Arbol a -> Bool
algunoArbol' p = foldArbol (\r bs -> p r || or bs) True

profundidad' :: Arbol a -> Integer
profundidad' = foldArbol (\r ls -> if ls /= [] then 1 + maximum ls else 1) 0

ocurrencias :: Eq a => a -> Arbol a -> Integer
ocurrencias n Vacio = 0
ocurrencias n (Nodo x xs)
	| n == x = 1 + sum (map (ocurrencias n) xs)
	|otherwise = 0 + sum ( map (ocurrencias n) xs)
	
ocurrencias' :: Eq a => a -> Arbol a -> Integer
ocurrencias' n = foldArbol (\r ss ->(if n==r then 1 else 0) + sum ss) 0

data ArbolB a = VacioB | NodoB (ArbolB a) a (ArbolB a) deriving Show

perteneceB :: Eq a => a -> ArbolB a -> Bool
perteneceB x VacioB = False
perteneceB x (NodoB i r d)
	| x==r = True
	| otherwise = perteneceB x i || perteneceB x d
	
a2 :: ArbolB Integer
a2 = NodoB a21 50 a22
	where
	a21 = NodoB (hojaB 30) 30 (hojaB 35)
	a22 = NodoB VacioB 60 (hojaB 80)
	hojaB x = NodoB VacioB x VacioB 

esArbolBB :: Ord a => ArbolB a -> Bool
esArbolBB VacioB = True
esArbolBB (NodoB i r d) = todosArbolB (<=r) i && todosArbolB (>r) d && esArbolBB i && esArbolBB d

todosArbolB :: (a -> Bool) -> ArbolB a -> Bool
todosArbolB p VacioB = True
todosArbolB p (NodoB i r d) = p r && todosArbolB p i && todosArbolB p d

perteneceBB :: Ord a => a -> ArbolB a -> Bool
perteneceBB x VacioB = False
perteneceBB x (NodoB i r d)
	| x==r = True
	| x < r = perteneceBB x i
	| otherwise = perteneceBB x d
	
insertarBB :: Ord a => a -> ArbolB a -> ArbolB a
insertarBB x VacioB = NodoB VacioB x VacioB
insertarBB x (NodoB i r d)
	|x <= r = NodoB (insertarBB x i) r d
	|otherwise = NodoB i r (insertarBB x d)
	
esVacioB :: ArbolB a -> Bool
esVacioB VacioB = True
esVacioB _ = False

eliminarBB :: Ord a => a -> ArbolB a -> ArbolB a 
eliminarBB x VacioB = VacioB
eliminarBB x (NodoB i r d)
	| x < r = NodoB (eliminarBB x i) r d 
	| x > r = NodoB i r (eliminarBB x d)
	| esVacioB i = d
	| esVacioB d = i
	| otherwise = NodoB i' mi d
		where
		(mi,i') = tomaMaxBB i 
		tomaMaxBB (NodoB i r VacioB) = (r,i)
		tomaMaxBB (NodoB i r d) = (m,NodoB i r d')
			where
			(m,d') = tomaMaxBB d
			
enOrden :: ArbolB a -> [a]
enOrden VacioB = []
enOrden (NodoB i r d) = enOrden i ++ (r : enOrden d)

enOrden' :: ArbolB a -> [a]
enOrden' x = aux x []
	where 
	aux :: ArbolB a -> [a] -> [a]
	aux VacioB xs = xs
	aux (NodoB i r d) xs = aux i (r : aux d xs)
	
listaAArbolBB :: Ord a => [a] -> ArbolB a 
listaAArbolBB = foldr insertarBB VacioB

treeSort :: Ord a => [a] -> [a]
treeSort = enOrden' . listaAArbolBB

lOrdAArbolBB :: Ord a => [a] -> ArbolB a 
lOrdAArbolBB [] = VacioB
lOrdAArbolBB xs = NodoB (lOrdAArbolBB ys) z (lOrdAArbolBB zs)
	where
	(ys,z:zs) = partir xs
	partir xs = splitAt (length xs `div` 2) xs

lOrdAArbolBB' :: Ord a => [a] -> ArbolB a
lOrdAArbolBB' = lOrdAArbolBB . treeSort

instance Functor ArbolB where
	fmap f VacioB = VacioB
	fmap f (NodoB i r d) = NodoB (fmap f i) (f r) (fmap f d)

foldArbolB :: (b -> a -> b -> b) -> b -> (ArbolB a -> b)
foldArbolB f e VacioB = e 
foldArbolB f e (NodoB i r d) = f (foldArbolB f e i) r (foldArbolB f e d)

perteneceB' :: Eq a => a -> ArbolB a -> Bool
perteneceB' x = foldArbolB (\pi r pd -> x == r ||pi||pd) False

todosArbolB' :: (a -> Bool) -> ArbolB a -> Bool
todosArbolB' p = foldArbolB (\ti r td -> p r && ti && td) True

{-type Array a = [a]

creaArray :: [a] -> Array a 
creaArray xs = xs 
ar :: Array Char
ar = creaArray ['a'..'d']

infixl 9 !
(!) :: Array a -> Integer -> a 
(x:_) ! 0 = x
a@(_:xs) ! n | n>0= xs ! (n-1)
_ ! _ = error "(!): Posicion no valida"

infixl 8 =:
(=:) :: Array a -> (Integer,a) -> Array a 
(x:xs) =: (0,y) = (y:xs)
a@(x:xs) =: (n,y) | n>0= x : (xs =: ((n-1),y))
_ =: _ = error "(=:) Posicion no valida"
-}

type Matriz = Array (Array Float)

matriz :: Int -> Matriz
matriz n = creaArray (replicate n creaFila)
	where creaFila = creaArray (replicate n 0.0)

data ArbolH a = HojaH a | NodoH (ArbolH a) (ArbolH a) deriving Show
type Array a = ArbolH a 

creaArray :: [a] -> Array a 
creaArray [x] = HojaH x 
creaArray xs@(_:_:_) = NodoH (creaArray i) (creaArray d)
	where 
	(i,d) = partir xs
	partir :: [a] -> ([a],[a])
	partir [] = ([],[])
	partir [x] = ([x],[])
	partir (x:y:zs) = (x:xs,y:ys)
		where (xs,ys) = partir zs 
		
xr :: Array Char
xr = creaArray ['a'..'d'] 

infixl 9 !
(!) :: Array a -> Integer -> a 
HojaH x ! 0 = x 
NodoH i d ! n = if even n then i ! (div n 2) else d ! (div n 2)
_ ! _ = error "posicion no valida"

infixl 8 =:
(=:) :: Array a -> (Integer,a) -> Array a 
HojaH x =: (0,y) = HojaH y 
NodoH i d =: (n,y) = if even n then NodoH (i =: (div n 2,y)) d else NodoH i (d =: (div n 2,y))
_ =: _ = error "Posicion no valida"

--data Grafo v = G [v] (v -> [v])

class Eq v => Grafo v where
	vertices :: [v]
	suc :: v -> [v]
	(</-) :: v -> [v] -> Bool
	caminoBFS :: v -> v -> [v] 
	caminoDFS :: v -> v -> [v] 
	caminoDesdeDFS :: v -> (v -> Bool) -> [v] -> [[v]]
	caminoDesdeBFS :: v -> (v -> Bool) -> [v] ->[v] -> [[v]]
	x </- ys = and [x /= y | y <- ys]
	caminoDFS u v = if (caminoDesdeDFS u (==v) [] ) /= [] then head (caminoDesdeDFS u (==v) [] ) else []
	caminoBFS u v = if (caminoDesdeBFS u (==v) [] [] ) /= [] then head (caminoDesdeBFS u (==v) [] [] ) else []
	caminoDesdeDFS o te vis 
		| te o = [o : vis]
		| otherwise = concat [caminoDesdeDFS o' te (o:vis) | o' <- suc o , o' </- vis]
	caminoDesdeBFS o te vis vis'
		| te o = [o : vis]
		| otherwise = concat [caminoDesdeBFS o' te (o:vis) (o:vis'++o'') | let o'' = suc o,o' <- (suc o) , o' </- vis']
data MiVertice = A|B|C|D|E|F|G|H|I deriving (Show,Eq,Enum)

instance Grafo MiVertice where
	vertices = [A .. I]
	suc A = [B,F]
	suc B = [C,G]
	suc C = [H]
	suc D = [C,E,H]
	suc E = []
	suc F = [E,I]
	suc G = [A]
	suc H = [G,I]
	suc I = [E,G]
	
{-instance Grafo MiVertice where	
	vertices = [A .. E]
	suc A = [B,C,D]
	suc B = [C]
	suc C = [A,D]
	suc D = [C]
	suc E = []
	-}
	--v </- vs = notElem v (vs \\ [v])
	--v </- vs = notElem v vs && v /= B
	
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = foldl elimDe
elimDe :: Eq a => [a] -> a -> [a]
elimDe [] x = []
elimDe (y:ys) x = if x==y then ys else y : elimDe ys x
	

	