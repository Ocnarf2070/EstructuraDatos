module WellBalanced where
import DataStructures.Stack.LinearStack
wellBalanced :: String -> Bool
wellBalanced xs = wellBalanced' xs empty
wellBalanced' :: String -> Stack Char -> Bool
wellBalanced' [] s = isEmpty s
wellBalanced' (x:xs) s 
	| elem x open = wellBalanced' xs $ push x s
	| elem x close = if (top s == open !! (pos x close)) then wellBalanced' xs (pop s) else False
	| otherwise = wellBalanced' xs s

open = "([{"
close = ")]}"

pos x [] = error "not index element"
pos x xs = aux x xs 0 where aux y ys n = if null ys then pos y ys else if y == head ys then n else aux y (tail ys) (n+1) 