--import Data.List

inc :: Integer -> Integer
inc x = x+1

sumaCuadrados :: Integer->Integer->Integer
sumaCuadrados x y = x^2+y^2

componer :: (Integer->Integer)->(Integer->Integer->Integer)->Integer->Integer->Integer
componer g f x y = g(f x y)

--desort = (reverse . sort)
--countdown = desort [2,8,7,10,1,9,5,3,4,6]

aEntero :: [Integer] -> Integer
aEntero [] = 0
aEntero (x:xs)=x*10^(fromIntegral $ length(xs))+aEntero xs

aLista :: Integer->[Integer]
aLista x = reverse $ aLista' x
aLista' 0 =[]
aLista' x =(mod x (10)): aLista'(div x (10))

sumaPares ::[(Integer,Integer)]->Integer
sumaPares []=0
sumaPares ((x,y):xs)=x+y+sumaPares xs

longitud :: [Integer]->Integer
longitud []=0
longitud (_:xs)=1+longitud xs

raicesR:: Float -> Float -> Float -> (Float,Float)
raicesR a b c 
	|b^2-4*a*c>=0 = ((-b+raizDisc)/denom,(-b-raizDisc)/denom)
	|otherwise = error "raíces complejas"
	where
		disc=b^2-4*a*c
		raizDisc = sqrt disc
		denom=2*a

descomponer :: Integer -> (Integer,Integer,Integer)
descomponer x = (h,min,seg)
	where
	seg=mod x 60
	min=mod (div x 60) 60
	h=div x 3600
-------------------------------------------------------------------------------------------
{-infix 4 |>
(|>) :: [Integer -> Integer]-> Integer -> [Integer]
[f] |> x= [f x]
f:fs |> x = f x : (fs |> x)
-}

esBisiesto :: Integer -> Bool
esBisiesto x
	|(mod x 4)==0 && (mod x 100)/=0 = True
	|(mod x 400)==0 = True
	|otherwise =False

diasMes :: Integer->Integer->Integer
diasMes 2 y |esBisiesto y = 29
		    |otherwise=28
diasMes x y |x==1||x==3||x==5||x==7||x==8||x==10||x==12 = 31
		    |otherwise = 30

aLaDerechaDe :: Integer -> Integer -> Integer			
aLaDerechaDe x y = x+y*10

restoDe :: Integer -> Integer -> Integer
restoDe x y
	|x<y = x
	|otherwise = restoDe (x-y) y

divididoPor :: Integer -> Integer -> Integer
divididoPor x y
	|x<y = 0
	|otherwise = 1 + divididoPor (x-y) y

sumaDesdeHasta a b 
	|a>b=0
	|a==b = a
	|otherwise = a + sumaDesdeHasta (inc a) b

prodDesdeHasta a b 
	|a>b = 1
	|a==b = a
	|otherwise = a * prodDesdeHasta  (a+1)	b

variaciones m n = div (prodDesdeHasta 1 m) (prodDesdeHasta 1 (m-n))
variaciones' :: Integer -> Integer -> Integer
variaciones' 1 m = m
variaciones' m n=(m-n+1)*variaciones' (n-1) m

--numCom m n|m < n = 0
numCom m n= div (prodDesdeHasta 1 m) ((prodDesdeHasta 1 (m-n))*(prodDesdeHasta 1 n))

numCom' :: Integer -> Integer -> Integer
numCom' m 0 = 1
numCom' m n |m==n = 1
numCom' m n = numCom' (m-1) (n-1) + numCom' (m-1) n

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n=fibonacci(n-1)+fibonacci(n-2)

mayor3 :: Integer->Integer->Integer -> Integer
mayor3 y z = max (max y z)

mayor4 :: Integer-> Integer->Integer->Integer->Integer
mayor4 x y z = max (max x (max y z))

ordena3 :: Integer->Integer->Integer->(Integer,Integer,Integer)
ordena3 x y z = ordena3'(x,y,z)
	where
	ordena3' (x,y,z)
		|x>y = ordena3' (y,x,z)
		|y>z = ordena3' (x,z,y)
		|otherwise = (x,y,z)
	
esCapicua :: Integer -> Bool
esCapicua x 
	|length lista /= 4 = error "ERROR: numero de cifras incorrecto"
	|otherwise = lista== reverse lista
	where 
	lista = aLista x
	
sumaCifras :: Integer -> Integer
sumaCifras x = sumar lista
	where
	lista = aLista x
	sumar []=0
	sumar (x:xs)=x+sumar xs

numeroCifras :: Integer -> Integer
numeroCifras x = toInteger $ length $ aLista x

trocear :: Integer -> (Integer,Integer)
trocear x = (divididoPor x 10,restoDe x 10) 

concatenar :: Integer -> Integer -> Integer
concatenar x y
	|y==0 = x 
	|otherwise = x * (decimal y) + y 
	where 
	decimal x = 10^(length$aLista x)
	
----------------------------------------------------------------------------------------------------------

logEnBase :: Float -> (Float -> Float)
logEnBase b = \x -> log x / log b

iter :: (Integer -> Integer -> Integer) -> Integer -> (Integer -> Integer)
iter op e = fun
	where 
	fun 0 = e
	fun n = op n (fun (n-1))
	
(|>) :: [a -> b] -> a -> [b]
(|>) = flip (map . flip ($))

data DíaSemana = Lunes|Martes|Miércoles|Jueves|Viernes|Sábado|Domingo deriving (Show,Enum)
ordDíaSemana :: [DíaSemana]
ordDíaSemana = [Lunes .. Domingo]

diaSemana :: Int -> Int -> Int -> DíaSemana
diaSemana d m a = ordDíaSemana !! ((700+(26*x-2)`div`10+d+y+y`div`4+z`div`4-2*z)`mod`7)
	where 
		x | m<=2 = m+10
		  | otherwise = m-2
		y | m<=2 = mod (a-1) 100
		  | otherwise = mod a 100
		z | m<=2 = div (a-1) 100
		  | otherwise = div a 100

type Radio=Float
type Lado=Float
data Figura = Circulo Radio | Cuadrado Lado | Rectangulo Lado Lado | Punto deriving Show

perimetroFig :: Figura -> Float
perimetroFig (Circulo x) = (*) (2*pi) x
perimetroFig (Cuadrado x) =4 * x
perimetroFig (Rectangulo x y)= 2 * x + 2 * y

areaFig :: Figura -> Float
areaFig (Circulo x)= pi * (x^2)
areaFig (Cuadrado x)= x * x
areaFig (Rectangulo x y)= x * y

data Complejo = Float :- Float deriving Show
data Resultado = UnaReal Float| DosReales Float Float| DosComplejas Complejo Complejo deriving Show
raices :: Float -> Float -> Float -> Resultado
raices a b c 
	|disc > 0 = DosReales ((-b+raizDisc)/denom) ((-b-raizDisc)/denom)
	|disc == 0 = UnaReal ((-b)/(2*a))
	|otherwise = DosComplejas ((-b/denom):-(raizDiscCom/denom)) ((-b/denom):- (-raizDiscCom/denom))
		where
		disc = b^2-4*a*c
		raizDisc = sqrt disc
		raizDiscCom = sqrt (-disc)
		denom = 2*a
		
data Nat = Cero | Suc Nat deriving (Show,Eq,Ord)

uno::Nat
uno = Suc Cero
dos::Nat
dos = Suc uno
tres = Suc dos
cuatro = Suc tres
cinco = Suc cuatro
seis = Suc cinco
siete = Suc seis
ocho = Suc siete
nueve = Suc ocho
indefinidoN :: Nat
indefinidoN = undefined
infinitoN :: Nat
infinitoN = Suc infinitoN

esPar :: Nat -> Bool
esPar Cero = True
esPar (Suc x)=not (esPar x)

infixl 6 <+>
(<+>) :: Nat -> Nat -> Nat
m <+> Cero = m
m <+> (Suc n) = Suc(m <+> n)

infixl 6 <->
(<->) :: Nat -> Nat -> Nat
m <-> Cero = m
Cero <-> m = indefinidoN
(Suc m) <-> (Suc n) = m <-> n 

infixl 7 <*>
(<*>)::Nat->Nat->Nat
--m<*>Cero= Cero
Cero <*> n = Cero
--m<*>(Suc n)=m<*>n<+>m
(Suc m) <*> n = m <*> n <+> n 

infixr 8 <^>
(<^>)::Nat->Nat->Nat
--b <^> Cero = uno
--b <^> (Suc n) = b<*>b<^>n
uno <^> n = uno
(Suc m) <^> n = uno <+> (sumBinNewNat n n m)

potencia m n 
	| m==1 = 1
	|otherwise = 1 + sumBinNew n n (m-1)

sumBinNew n i x 
	| n==0 = 0
	| i==1 = (numCom' n i)*x
	|otherwise = (sumBinNew n (i-1) x) + ((numCom' n i)*(potencia x i))

combNat :: Nat -> Nat -> Nat
combNat _ Cero = uno
combNat m@(Suc _) n@(Suc _)
	|m==n = uno
	|otherwise = combNat (m<->uno) (n<->uno) <+> combNat (m<->uno) n
	
sumBinNewNat n@(Suc _) i@(Suc l) x@(Suc _)
	|i==uno = (combNat n i) <*> x
	|otherwise =(sumBinNewNat n l x) <+> ((combNat n i) <*> (x<^>i))

toNat :: Integer -> Nat
toNat x |x<0 = error "numero negativo"
toNat 0 = Cero
toNat x = Suc (toNat (x-1))

repLin :: Nat -> String
repLin Cero = []
repLin (Suc n)='|':(repLin n)

divNat :: Nat -> Nat -> Nat
divNat m Cero = error "Division entre cero"
divNat Cero n = Cero
divNat m@(Suc _) n@(Suc _) 
	| m < n = Cero
	| otherwise = uno <+> (divNat (m <-> n) n)
	
modNat :: Nat -> Nat -> Nat
modNat m Cero = error "Division entre cero"
modNat Cero n = Cero
modNat m@(Suc _) n@(Suc _) 
	| m < n = m
	| otherwise = (modNat (m <-> n) n)
	
------------------------------------------------------------------------------