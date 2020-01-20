import System.Process
import Data.List
clear = system "cls"
desde :: Integer -> [Integer]
desde x = x : desde (x+1)

suma :: Integer -> [Integer] -> Integer
suma n (x:xs) = if n==0 then 0 else x + suma (n-1) xs

selec n (x:xs) = if n==1 then x else selec (n-1) xs

selecHasta n (x:xs) = if n==1 then [x] else x : selecHasta (n-1) xs 

mezclar2Listas [] [] = []
mezclar2Listas xs [] = xs
mezclar2Listas [] ys = ys
mezclar2Listas (x:xs) (y:ys) = x : y : mezclar2Listas xs ys

combFun :: (a -> a) -> Integer -> a -> a
combFun f 0 a = a
combFun f n a = f $ combFun f (n-1) a

lista f a = mezclar2Listas ([x|n<-[1,3..],x<-(combFun f n a):[]]) ([x|n<-[0,2..],x<-(combFun f n a):[]])

fun a = 3 * a - 2
gar a = (a/360)*(2*pi)
rag a = (a/(2*pi))*360
aLaDerechaDe :: Integer -> Integer -> Integer			
aLaDerechaDe x y = x+y*10

ordena [] = []
ordena (x:xs) = ordena [y|y <- xs, y<x] ++ x : ordena [y|y <- xs , y >= x]

divisores n = [i|i <- [1 .. n-1], mod n i == 0]
perfecto n = sum(divisores n) == n
perfectos = filter perfecto [1..]

incr :: [Int] -> [Int]
incr [] = []
incr (n:ns) = (n+1):incr ns

añadeCero :: [Int] -> [Int]
añadeCero = (0:)

pos' = e where (i,e)=(incr e, añadeCero i)

criba :: [Integer] -> [Integer]
criba (p:xs) = [x|x <- xs, noDivideA p x]
	where noDivideA m n = mod n m /= 0
	
primos :: [Integer]
primos = map head (iterate criba [2..])

pascal :: [[Integer]]
pascal = [1] : map f pascal
	where f cs = zipWith (+) (0:cs) (cs ++ [0])
	
fib n = aux 1 0 n 
	where aux ac1 ac2 n 
		|n==0 = ac1
		|otherwise = aux (ac1+ac2) ac1 (n-1)
		
mcd = gcd
mcm = lcm

comprobacion n m = mcd (fib n) (fib m) == fib (mcd n m)

yn = 1:1:zipWith (+) (zipWith (*) [0..] (tail yn)) (map (*3) yn)

yn' = sol where
			sol = 1: sol1
			sol1 = 1: sol2
			sol2 =zipWith (+) sol3 (map (*3) sol)
			sol3 = zipWith (*) [0..] sol1
			
mezcla u v w = (u <|> v) <|> w 
	where
		a@(x:xs) <|> b@(y:ys)
			|x==y = x: (xs <|> ys)
			|x<y = x: (xs <|> b)
			|y<x = y: (a <|> ys)
			
h = 1 : mezcla (map (2*) h) (map (3*) h) (map (5*) h)

auto = [1,2] ++ (sig [2] 2)
sig (x:xs) u = x: sig (xs ++ nuevos) nu
	where 
		nu = if u==1 then 2 else 1
		nuevos = if x==2 then [nu,nu] else [nu]

cantidad :: Eq a => [a] -> a -> Integer		
cantidad [] n = 0
cantidad (x:xs) n 
	|x==n = 1 + cantidad xs n
	|otherwise = 0 + cantidad xs n

cantidadHasta :: Eq a => Integer -> [a] -> a -> Integer		
cantidadHasta _ [] _ = 0
cantidadHasta 0 _ _ = 0
cantidadHasta l (x:xs) n 
	|x==n = 1 + cantidadHasta (l-1) xs n
	|otherwise = 0 + cantidadHasta (l-1) xs n	
 
distAutoHasta n = (u,v)
	where 
		u = fromInteger(cantidadHasta n auto 1) * (100/fromInteger n)
		v = fromInteger(cantidadHasta n auto 2) * (100/fromInteger n)
		
genera :: [Int] -> [Int]
genera xs = gen xs [1,2]

gen :: [Int] -> [Int] -> [Int]
gen (c : cs) [u,v] = repite c u ++ gen cs [v,u]
	where 
	repite 1 u = [u]
	repite 2 u = [u,u]
		
auto' = 1 : 2 : u where u = 2 : gen u [1,2]
auto''@(1:2:r) = genera auto''

contadora :: [Int] -> [Int]
contadora (c:cs) = contadora' c cs 1
	where
		contadora' _ [] n = [n]
		contadora' c (d:ds) n 
			| c == d = contadora' d ds (n+1)
			|otherwise = [n] ++ contadora' d ds 1

			
comprime xs = comp xs 1
comp (x:y:xs) n 
	|x==y = comp (y:xs) (n+1)
	|otherwise = n :x : comp (y:xs) 1
comp [x] n = [n,x]

expande [] = []
expande (x:y:xs) = repite x y ++ expande xs
	where repite n y = take n (repeat y)
	
--l = takeWhile ((>) 100 . length) [1..300]
------------------------------------------------------------------------------------
d f g = nub $ d' f g
d' f g=if ((1 < f 1) && (1 < g 1)) then 1 : mezcla2 (map f (d' f g)) (map g (d' f g)) else error "Las funciones no verifican la regla de x < f (x) y x < g (x) "

mezcla2 u v = (u <|> v)
	where
		a@(x:xs) <|> b@(y:ys)
			|x==y = x: (xs <|> ys)
			|x<y = x: (xs <|> b)
			|y<x = y: (a <|> ys)

ver f g xs = foldr (\x -> (&&)((x < f x)&&(x < g x))) True xs


loca :: [Int] -> [Int]
loca xs = r
	where
	(r,m) = foldr f ([],0) xs
	f = \x (s,y) -> (m : s, max x y)
	
type Par = (Int,Int)
menorPar :: Par -> Par -> Bool
menorPar a@(x,y) b@(u,v) 
	| (x + y) == (u + v) = y < v
	| (x + y) < (u + v) = True
	| otherwise = False
	
inx (x,y) = (succ x,y)
iny (x,y) = (x,succ y)
mezcla' :: [Par] -> [Par] -> [Par]
mezcla' u v = u <|> v
		where 
		a@(x:xs) <|> b@(y:ys)
			|x==y = x: (xs <|> ys)
			|menorPar x y = x: (xs <|> b)
			|not $ menorPar x y = y: (a <|> ys)
pos = (0,0) : mezcla' (map inx pos) (map iny pos)

sumaPar (x,y) = x + y

listaPerm xs = and $ map (\x -> elem x xs) [1..length xs]

ordena' [] = []
ordena' l@(x:xs) = ordena' (menores l) ++ [x] ++ ordena' (mayores l)
	where
	menores (x@(n,_):xs) = [x|x@(m,_) <- xs , m<n]
	mayores (x@(n,_):xs) = [x|x@(m,_) <- xs , m>=n]

infixr 5 >!>
(>!>) :: [Int] -> [a] -> [a]
(>!>) [] [] = []
(>!>) xs ys | (length xs == length ys) && listaPerm xs = get2 $ unzip $ ordena' $ zip xs ys
	where get2 (_,x) = x
(>!>) xs _ | not $ listaPerm xs = error "No es una lista valida de permutaciones"
(>!>) _ _ = error "Tamaño diferentes"

pos'' ::Eq a=> a -> [a] -> Int
pos'' c (x:xs) 
	| c == x = 1
	| otherwise = 1 + pos'' c xs

infixr 5 <!<
(<!<) :: Eq a => [a] -> [a] -> [Int]
(<!<) [] [] = []
(<!<) xs ys | (length xs == length ys) = op xs ys
	where 
	op [] _ = []
	op (x:xs) ys =  pos'' x ys : xs `op` ys
(<!<) _ _ = error "Tamaño diferentes"

infixr 5 <>
(<>) :: Integral a => a -> a -> a -> [a]
(a <> b) n | a < b && b < n = op a b 1 n
	where
	op a b cont n 
		|n < cont = []
		|cont == a = b : op a b suc n
		|cont == b = a : op a b suc n
		|cont == n = [n]
		|otherwise = cont : op a b suc n
			where suc = cont + 1
		
(_ <> _) _ = error "Mala formulacion del operador:sea a,b,n :(a op b) n -> a < b , b < n"