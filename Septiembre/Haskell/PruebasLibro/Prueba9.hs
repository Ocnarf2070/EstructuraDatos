import Data.Char

inv :: [a] -> [a]
inv = flip inv2 []
inv2 [] ys = ys
inv2 (x:xs) ys = inv2 xs (x:ys)

data ArbolH a = HojaH a | NodoH (ArbolH a) (ArbolH a) deriving Show

a1 :: ArbolH Integer
a1 = NodoH (NodoH (NodoH (HojaH 2) (HojaH 3)) (HojaH 5)) (HojaH 7)

profundidadH :: ArbolH a -> Integer
profundidadH (HojaH _) = 1
profundidadH (NodoH i d) = 1 + max (profundidadH i) (profundidadH d)

tamañoH :: ArbolH a -> Integer
tamañoH (HojaH _) = 1
tamañoH (NodoH i d) = tamañoH i + tamañoH d

perteneceH ::Eq a => a -> ArbolH a -> Bool
perteneceH a (HojaH x) = a == x
perteneceH a (NodoH i d) = perteneceH a i || perteneceH a d

todosArbolH :: (a -> Bool) -> ArbolH a -> Bool
todosArbolH f (HojaH n) = f n
todosArbolH f (NodoH i d) = todosArbolH f i && todosArbolH f d

instance Functor ArbolH where
	--fmap f (HojaH x) = HojaH (f x)
	--fmap f (NodoH i d) = NodoH (fmap f i) (fmap f d)
	fmap f = foldArbolH (\x y -> NodoH x y) (\z -> HojaH (f z))
	
foldArbolH :: (b -> b -> b) -> (a -> b) -> ArbolH a -> b
foldArbolH f g (HojaH x) = g x
foldArbolH f g (NodoH i d) = f (foldArbolH f g i) (foldArbolH f g d)

profundidadH' :: ArbolH a -> Integer
profundidadH' = foldArbolH (\y z-> 1 + max y z) (\x -> 1)

tamañoH' :: ArbolH a -> Integer
tamañoH' = foldArbolH (+) (\z -> 1) 

perteneceH' :: Eq a => a -> ArbolH a -> Bool
perteneceH' x = foldArbolH (||) (==x)

todosArbolH' :: (a -> Bool) -> ArbolH a -> Bool
todosArbolH' f = foldArbolH (&&) f

class TieneMaximo f where
	maximo :: Ord a => f a -> a
	
instance TieneMaximo [] where
	maximo [] = error "Lista vacia"
	maximo [x] = x
	maximo (x:xs) = max x (maximo xs)
	
instance TieneMaximo ArbolH where
	maximo (HojaH x) = x
	maximo (NodoH i d) = max (maximo i) (maximo d)
	
class TieneTamaño f where
	tamaño :: f a -> Integer 
	
instance TieneTamaño [] where
	tamaño [] = 0
	tamaño (_:xs) = 1 + tamaño xs
	
instance TieneTamaño ArbolH where
	tamaño = tamañoH'

data Arb c = V | N c [Arb c] deriving Show
a2 :: Arb Integer
a2 = N 1 [a21,a22]
	where
	a21 = N 2 [a211,a212,a213]
	a211= N 3 []
	a212= N 5 []
	a213= N 7 []
	a22= N 4 [a221,a222,a223,a224]
	a221= N 8 []
	a222= N 10 []
	a223= N 12 []
	a224= N 14 []

reduce :: ([b] -> c -> b) -> Arb c -> b -> b
reduce g V z  = z
reduce g (N x xs) z = g (map (\ys -> reduce g ys z) xs) x

aplica :: (c -> b) -> Arb c -> Arb b
aplica f t = reduce (\ss r -> N (f r) ss) t V  

prof :: Arb c -> Int
prof f = reduce (\ls r -> if ls /= [] then 1 + maximum ls else 1) f 0

-- visita :: Arb c -> [c]
-- visita t = reduce (\ ls r -> zipWith (++) [r] [ls] ) t []

type Dic = Arb Char

instance Functor Arb where
	fmap f V = V
	fmap f (N c xs) = N (f c) [fmap f x | x <- xs]
dic1 :: Dic
dic1 = N '.' [N 'a' [N 'l' [V, N 'a' [V]]
					,N 'n' [N 'a' [V]]
					,N 's' [N 'a' [V], N 'o' [V]]]
			 ,N 't' [N 'e' [V], N 'u' [V]]]	

pals :: Dic -> [String]
pals (N '.' xs) = concat (map (pals) xs )
pals V = [""]
pals (N c xs) = map (c :) (concat (map (pals) xs )) ++ []

aMayus :: Dic -> Dic
aMayus = fmap toUpper

estaEn :: String -> Dic -> Bool
estaEn p d = elem p (pals d)

-- (<:) :: Dic -> String -> Dic
-- (N '.' []) <: (p:ps) = N '.' [N p [V]] <: ps
-- (N '.' xs) <: ps = aux xs ps
	


data Huf a = Hoja (a, Int) | Nodo (Huf a) Int (Huf a) deriving (Show,Eq)
as1 :: [Huf Char]
as1 = [Hoja ('e',4), Hoja ('d',15), Hoja ('c',21), Hoja ('b',25), Hoja ('a',35) ]

instance Eq a => Ord (Huf a) where
	(Hoja (_,n)) <= (Hoja(_,m)) = n <= m 
	(Hoja (_,n)) <= (Nodo _ m _) = n <= m
	(Nodo _ m _) <= (Hoja (_,n))= m <= n
	(Nodo _ n _) <= (Nodo _ m _) = n <= m

(<+>) :: Huf a -> Huf a -> Int
(Hoja (_,n)) <+> (Hoja (_,m)) = n + m
(Hoja (_,n)) <+> (Nodo _ m _) = n + m
(Nodo _ m _) <+> (Hoja (_,n))= n + m
(Nodo _ n _) <+> (Nodo _ m _) = n + m

ordList :: Ord a =>  a -> [a] -> [a]
ordList x [] = [x]
ordList x (y:ys) = if x > y then y : ordList x ys else x : y : ys

ordHuf x [] = [x]
ordHuf x (y:ys)
	| x > y = y : ordHuf x ys
	| otherwise = x : y : ys

arbHuf :: Eq a => [Huf a] -> Huf a
arbHuf [x] = x
arbHuf (x:y:ys) 
	| length ys == 0 = arbHuf $ ordHuf (Nodo x (x <+> y) y) []
	| otherwise = arbHuf $ ordHuf (Nodo x (x <+> y) y) ys
	where
		ordHuf x [] = [x]
		ordHuf x (y:ys)
			| x > y = y : ordHuf x ys
			| otherwise = x: y : ys

codHuf :: Huf a -> [(a,String)]			
codHuf hf = codHuf' hf []
	where
	codHuf' :: Huf a -> String ->  [(a,String)]
	codHuf' (Nodo i _ d) xs = (codHuf' i (xs ++ "0")) ++ (codHuf' d (xs ++ "1"))
	codHuf' (Hoja (c,_)) xs = [(c,xs)]
	
codPal :: Eq a => [a] -> [(a,String)] -> String
codPal [] xs = []
codPal (s:ss) xs = busCar s xs ++ codPal ss xs
	where 
	busCar s [] = error "No hay codificacion a esta palabra"
	busCar s (x@(c,ys) : xs)
		| s == c = ys
		| otherwise = busCar s xs
		
descodPal :: String -> Huf a -> [a]
descodPal cod hf = descodPal' cod hf hf
	where
	descodPal' :: String -> Huf a -> Huf a -> [a]
	descodPal' [] (Hoja (c,_)) _= [c]
	descodPal' [] _ _= error "Mala codificacion"
	descodPal' (x : xs) (Nodo i _ d) hf = if x == '0' then descodPal' xs i hf else if x == '1' then descodPal' xs d hf else error "No es posible descodificar: algun valor no es 0 o 1"
	descodPal' xs (Hoja (c,_)) hf = c : descodPal' xs hf hf

	
arbHufFra :: String -> Huf Char
arbHufFra fr = arbHuf list
	where
	list =  ordListHoja fr
	
codHufFra :: String -> [(Char,String)]
codHufFra fr = codHuf $ arbHufFra fr

codFra :: String -> String
codFra fr = codPal fr $ codHufFra fr

listCarPal pal = listCarPal' pal []
	where
	listCarPal' [] xs = inv xs
	listCarPal' (x:xs) ys
		| not $ elem x ys = listCarPal' xs (x:ys)
		| otherwise = listCarPal' xs ys
		
numCarPal pal =  zip list (contCar pal)  
	where
	list = listCarPal pal
	contCar pal = map (\c ->contCar' c pal) list
		where
		contCar' c [] = 0
		contCar' c (l:ls)
			| c == l = 1 + contCar' c ls
			| otherwise = contCar' c ls

listHoja pal = map (\cs -> Hoja cs) (numCarPal pal)

ordListHoja pal = foldr (orden) []  (listHoja pal)
	where
	orden h [] = [h]
	orden h (y : ys) 
		| h > y =  y : orden h ys
		| otherwise = h : y :ys


data Grafo a = G [a] (a -> [a])

g1 = G [1 .. 5] suc
	where
	suc 1 = [2,3]
	suc 2 = [1,4]
	suc 3 = [1,4]
	suc 4 = [2,3]
	suc 5 = []
	


--gradoDe 