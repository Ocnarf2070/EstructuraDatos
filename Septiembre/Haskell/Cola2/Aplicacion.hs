import BCola
import TCola
import Cola

pBQ :: [a] -> BCola a
pBQ = foldr (flip meteEnCola) colaVacia

usoc xs =	
	let	qt = foldl meteEnCola (colaVacia :: TCola Int) xs
		qb = pBQ xs
		(e,qb') = sacaDeCola qb
		(e',qt') = sacaDeCola qt
		qb'' = meteEnCola qb' e
	in (qb'',qt')