data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving (Show,Enum)

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

instance Ord Nat where
	Cero <= _ = True
	Suc x <= Suc y = x <= y
	_ <= _ = False

instance Show Nat where
--	showsPrec _ x = (shows . fromEnum) x
	showsPrec _ Cero = showChar ' '
	showsPrec _ (Suc x) = (showChar '|') . (shows x)

instance Enum Nat where 
	fromEnum Cero = 0
	fromEnum (Suc n) = 1 + fromEnum n
	toEnum 0 = Cero
	toEnum x = Suc (toEnum (x-1))
	
toNat :: Integer -> Nat
toNat 0 = Cero
toNat x = Suc (toNat (x-1))

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
		
		
reverse ::[a] -> [a]
reverse xs = revOn xs []
	where
	revOn [] ys = ys
	revOn (x:xs) ys = revOn xs (x:ys)
{-	
init [x]=[]
init (x:xs) = x : Main.init xs
	
last [x]=x
last (_:xs)=Main.last xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) | n>0 = Main.drop (n-1) xs
drop _ _ = error "Pelude.take: negative argument"

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p ys@(x:xs)
	| p x = Main.dropWhile p xs
	| otherwise = ys
	-}

divideA :: Integer -> Integer -> Bool
d `divideA` n = mod n d == 0

divisoresDe :: Integer -> [Integer]
divisoresDe n = [x|x<-[1..n], x `divideA` n]

mcd :: Integer -> Integer -> Integer
mcd a b = maximum [x|x <- divisoresDe a, x `divideA` b]

esPrimo :: Integer -> Bool
esPrimo n = divisoresDe n == [1,n]

losPrimos :: [Integer]
losPrimos = [n|n<-[2..], esPrimo n]

primeroQue :: (a -> Bool) -> [a] -> a
primeroQue f xs = head $ dropWhile (\x -> (not . f) x) xs

divisoresDe' :: Integer -> [Integer]
divisoresDe' n = [x|x<-[1..n-1], x `divideA` n]

numPerf :: [Integer]
numPerf = [x|x<-[1..],x == sum (divisoresDe' x)]

ternasPitHasta :: (Enum a,Real a) => a -> [(a,a,a)]
ternasPitHasta n = [(x,y,z)|let ns=[1..n],x<-ns,y<-ns,z<-ns,x^2+y^2==z^2] 

conmuta :: Eq b => (a -> a -> b) -> [a] -> Bool
conmuta f ds = and' [a `f` b == b `f` a | a <- ds, b <- ds]

and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

all' :: (a -> Bool) -> [a] -> Bool
all' f = (and' . map f)

any' :: (a -> Bool) -> [a] -> Bool
any' f = (or' . map f)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)

maximum' :: Ord a => [a] -> a
maximum' = foldr1' (max)

minimun' :: Ord a => [a] -> a
minimun' = foldr1' (min)

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f ys@(x:xs) = fun f (Main.reverse ys)
	where
	fun f [x] = x
	fun f (x:xs) = f (fun f xs) x 
	
factorial :: Integer -> [Integer] 
factorial n = scanl (*) 1 [2..n]
factorial' :: Integer -> [Integer] 
factorial' n = Main.reverse $ scanr (*) 1 (Main.reverse [2..10])

ordenadaAsc :: Ord a => [a] -> Bool
ordenadaAsc ys = and'[x|n<-[0,1..length ys-2], x<-[(init ys!!n)<=(tail ys!!n)]]
--ordenadaAsc xs = init xs <= tail xs

ordenadaPor :: (a -> a -> Bool) -> [a] -> Bool
ordenadaPor f ys = and'[x|n<-[0,1..length ys-2], x<-[(init ys!!n)`f`(tail ys!!n)]]

insertar :: Ord a => a -> [a] ->[a]
insertar x [] = [x]
insertar x ls@(y:ys)
	|x <= y = x:ls
	|otherwise = y : insertar x ys
	
ordenarIns :: Ord a => [a] -> [a]
ordenarIns = foldr insertar []

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails ys@(_:xs) = ys : tails xs

segs :: [a] -> [[a]]
segs [] = [[]]
segs ls@(x:xs) = segs xs ++ (tail . inits) ls

partes xs = [x|n<-[0..length xs],x<-ordenarIns(filter (\ys -> n==length ys) (segs xs))]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (intercala x) (perms xs))

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x ls@(y:ys) = (x:ls) : map (y:) (intercala x ys)

span' p xs = ((takeWhile p xs),(dropWhile p xs))
break' p = span (not . p)

repBin :: Integer -> [Char]
repBin n = Main.reverse $ rB n
	where
	rB 0 = '0':[]
	rB 1 = '1':[]
	rB n = (rB (mod n 2) ++ rB (div n 2))
	
inv :: [a] -> [a]
inv = flip inv2 []
inv2 [] ys = ys
inv2 (x:xs) ys = inv2 xs (x:ys)

esCapicua :: Eq a => [a] -> Bool
esCapicua xs 
	|mod (length xs) 2 /= 0 = (take (div (length xs) 2) xs) == (inv (drop (div (length xs) 2 + 1) xs))
	|otherwise = (take (div (length xs) 2) xs) == (inv (drop (div (length xs) 2) xs))
	
de :: Int -> [a] -> a
de _ [] = error "index too large"
de 1 (x:_) = x
de n (_:xs) = de (n-1) xs

circulaD :: [a] -> [a]
circulaD xs = last xs : init xs

circulaI :: [a] -> [a]
circulaI xs = tail xs ++ head xs : []

caracter :: Integer -> String -> Char
caracter n xs = xs !! (fromInteger n)

pos :: Char -> String -> Integer
pos c (x:xs) 
	| c == x = 0
	| otherwise = 1 + pos c xs

esAnagrama :: Eq a => [a] -> [a] -> Bool
esAnagrama xs ys |length xs == length ys = elem xs (perms ys)
				 | otherwise = False

posPermAna xs ys = lugar ys (perms xs)
	where 
	lugar o (x:xs)
		| o == x = 0
		| otherwise = 1 + lugar o xs
	
trasponer xs ys zs 
	|(length xs==length ys) && (esAnagrama ys zs) = (perms xs) !! (posPermAna ys zs)
	| esAnagrama ys zs == False = error "el patron no es entre ellos anagrama"
	|otherwise = error "no tienen el mismo tamaño"
	
anade xs c = inv $ c : inv xs
 
--k [] c = [c]
--k (x:xs) c = x : k xs c

data Empleado = P ([Char],Integer) deriving (Show,Eq,Ord)

joven :: Empleado -> Empleado -> Empleado
joven p1@(P(_,x)) p2@(P(_,y)) 
	| x < y = p1
	|otherwise = p2

masJoven :: [Empleado] -> Empleado
masJoven = foldr1 (joven)
