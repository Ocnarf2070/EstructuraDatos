potencia :: Integer -> Integer -> Integer
potencia x y |(y==1) = x 
			 |otherwise = x*potencia x (y-1)

potencia' :: Integer -> Integer -> Integer
potencia' x y 
	| (y==1) = x -- caso inicial para la primera parte de la ecuacion en partes
	| (y==2) = x*x -- caso inicial para la segunda parte de la ecuacion en partes
	| ((mod y 2)==0) = potencia' (potencia' x (div y 2)) 2
	| otherwise = x*potencia' (potencia' x (div (y-1) 2)) 2