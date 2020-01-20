module UsoCola where
import Cola

usoc xs =
	let	q = foldl meteEnCola colaVacia xs
		(e,q') = sacaDeCola q
		q'' = meteEnCola q' e
	in q''