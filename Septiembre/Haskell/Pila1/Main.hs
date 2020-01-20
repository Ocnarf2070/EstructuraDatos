import Pila

palindromo :: String -> Bool
palindromo pal = iguales (foldl meteEnPila pilaVacia (take (div (length pal) 2) pal)) (if mod (length pal) 2 == 0 then drop (div (length pal) 2) pal else drop ((div (length pal) 2)+1) pal )
	where
		iguales p [] | estaVaciaPila p = False
		iguales p [x] = (topeDePila p) == x
		iguales p pal = a == (head pal) && iguales ps (tail pal)
			where
			(a,ps) = sacaDePila p
			
insOrd :: (Eq a,Ord a) => a -> [a] -> [a]
insOrd x [] = [x]
insOrd x xs = insOrd' x (take (div (length xs) 2) xs) (drop (div (length xs) 2) xs)
	where
		insOrd' ::(Eq a, Ord a) => a -> [a] -> [a] -> [a]
		insOrd' x [] [d]
			| x <= d = x : [d]
			| otherwise = [d] ++ [x]
		insOrd' x i d
			| (last i) < x && x < (head d) = i ++ x : d
			|(head d) == x = i ++ x : d
			|x > (head d)= i ++ insOrd' x (take (div (length d) 2) d) (drop (div (length d) 2) d)
			|otherwise = insOrd' x (take (div (length i) 2) i) (drop (div (length i) 2) i) ++ d
		