type Complejo = (Char,Float,Float)

gar a = (a/360)*(2*pi)
rag a = (a/(2*pi))*360

nc1 :: Complejo
nc1=('c',1,0)


ang :: RealFloat a => a -> a -> a
ang x y
	|y<0 = (2*pi) + atan2 y x
	|otherwise = atan2 y x

isPol :: Complejo -> Bool
isPol x@('c',_,_) = False
isPol ('p',_,y) 
				| y<0 = False
				| otherwise = True
				
	
toPol :: Complejo -> Complejo
toPol x@('p',_,_)|isPol x = x
				 |otherwise = error "Mal definicion de complejos en forma polar"
toPol ('c',x,y)=('p',sqrt $ (+) (x^2) (y^2),(ang x y))

toCart :: Complejo -> Complejo
toCart x@('c',_,_)=x
toCart z@('p',x,y)
			| isPol z = ('c',x*cos y,x*sin y)
			| otherwise = error "Mal definicion de complejos en forma polar"
			
infixl 6 <+> 
(<+>) :: Complejo -> Complejo -> Complejo
x@('p',_,_) <+> y@('p',_,_) | isPol x && isPol y = toPol $ toCart x <+> y
x@('p',_,_) <+> y |isPol x= toCart x <+> y
x <+> y@('p',_,_) | isPol y= toCart y <+> x
('c',a,b) <+> ('c',c,d) = ('c',a+c,b+d)
x<+>y = error "forma polar de compleja no valida"

infixl 6 <-> 
(<->) :: Complejo -> Complejo -> Complejo
x@('p',_,_) <-> y@('p',_,_) | isPol x && isPol y = toPol $ toCart x <-> y
x@('p',_,_) <-> y | isPol x= toCart x <-> y
x <-> y@('p',_,_) |isPol y = x <-> toCart y 
('c',a,b) <-> ('c',c,d) = ('c',a-c,b-d)
x<->y = error "forma polar de compleja no valida"

infixl 7 <*>
(<*>) :: Complejo -> Complejo -> Complejo
x@('p',a,b) <*> y@('p',c,d) | isPol x && isPol y= ('p',a*c,b+d)
x@('p',_,_) <*> y | isPol x = toCart x <*> y
x <*> y@('p',_,_) | isPol y=toCart y <*> x
('c',a,b) <*> ('c',c,d) = ('c',a*c-b*d,a*d+b*c)
x<*>y = error "forma polar de compleja no valida"

infixl 7 </>
(</>) :: Complejo -> Complejo -> Complejo
x@('p',a,b) </> y@('p',c,d) | isPol x && isPol y= ('p',a/c,b-d)
x@('p',_,_) </> y | isPol x = toCart x </> y
x </> y@('p',_,_) | isPol y=toCart y </> x
('c',a,b) </> ('c',c,d) = ('c',(a*c+b*d)/(c^^2 + d^^2),(b*c-a*d)/(c^^2 + d^^2))
x</>y = error "forma polar de compleja no valida"

infixl 8 <^>
(<^>) :: Complejo -> Float -> Complejo
x@('p',a,b) <^> n | isPol x = ('p',a**n,b*n)
x@('c',_,_) <^> n = toCart $ toPol x <^> n
x <^> n = error "forma polar de compleja no valida"

aEntero :: Integral a => [a] -> a
aEntero [] = 0
aEntero (x:xs)=x*10^(length(xs))+aEntero xs

data Numero = Digito Int| Numero :^: Numero deriving Show


class SumaDig a where
	suma::a->Int

instance SumaDig Numero where
	suma (Digito x)=x
	suma (x :^: y)=suma x + suma y
	
elem' x [] = False
elem' x (y:ys) = if (x==y) then True else elem' x ys

------------------------------------------------------------------------------------

data Arbol a = Vacio | Hoja a | Nodo (Arbol a) a (Arbol a)
	deriving (Eq,Ord)
instance Show a => Show (Arbol a) where
	show Vacio = "o"
	show (Hoja x) = show x
	show (Nodo i x d) = "<"++show i ++"|"++show x ++"|"++show d++">"

class TiposConNormalizacion b where norm :: b -> b

infix 5 :/
data Rac a = a :/ a

instance Show a => Show (Rac a) where
	showsPrec _ (x :/ y)=shows x . showChar '/' . shows y
	
instance (Num a,Eq a) =>Eq (Rac a) where 
	(x :/ y) == (a :/ b) = x * b == a * y
	
instance Integral a => TiposConNormalizacion (Rac a) where
	norm (x :/ 0) = error "Racional con denominador 0"
	norm (x :/ y) = ((signum (x*y))*((abs x) `div` m)) :/ ((abs y) `div` m) where m = gcd x y
	
instance Integral a =>  Num (Rac a) where
	(x :/ y) + (x' :/ y') = norm ((x*y'+y*x') :/ (y*y'))
	(x :/ y) - (x' :/ y') = (x :/ y) + (negate x' :/ y')
	(x :/ y) * (x' :/ y') = norm ((x*x') :/ (y*y'))
	abs (x :/ y) = abs x :/ abs y
	fromInteger i = (fromInteger i) :/ 1
	signum a@(x :/ y)
		 | x == 0 = 0
		 | y == 0 = norm a
		 | x < 0 || y < 0 = -1
		 | otherwise = 1

instance (Integral a, Ord a) => Ord(Rac a) where 
	u<=v = x*y' <= y*x'
		where
		(x :/ y) = norm u
		(x' :/ y') = norm v
		
data Nat = Cero | Suc Nat deriving Eq

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

toNum :: Nat -> Integer
toNum Cero = 0
toNum (Suc n) = 1 + toNum n

toNat :: Integer -> Nat
toNat 0 = Cero
toNat x = Suc (toNat (x-1))

instance Ord Nat where
	Cero <= _ = True
	Suc x <= Suc y = x <= y
	_ <= _ = False

instance Num Nat where
	x + Cero = x
	x + Suc y = Suc (x + y)
	x - Cero = x
	Cero - x = indefinidoN
	Suc x - Suc y = x - y
	Cero * x = Cero
	(Suc x) * y = x * y + y
	abs x = x
	fromInteger i = toNat i
	signum i 
		| i == Cero = 0
		| otherwise = 1
		
instance Show Nat where
	showsPrec _ x = (shows . toNum) x
	{-showsPrec _ Cero = showChar ' '
	showsPrec _ (Suc x) = (showChar '|') . (shows x)-}
	