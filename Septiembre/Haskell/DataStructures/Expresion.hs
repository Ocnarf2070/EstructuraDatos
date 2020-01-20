import Data.List(intersperse)
import Data.Char

data Expr = Value Integer
 | Add Expr Expr
 | Diff Expr Expr
 | Mult Expr Expr
 deriving Show 
 
e1 :: Expr
e1 = Mult (Add (Value 1) (Value 2)) (Value 3)

evaluate :: Expr -> Integer
evaluate (Value v) = v
evaluate (Add x y) = (+) (evaluate x) (evaluate y)
evaluate (Diff x y) = (-) (evaluate x) (evaluate y)
evaluate (Mult x y) = (*) (evaluate x) (evaluate y)

toRPN exp = intersperse ' ' (postOrder exp)


inOrder exp  = aux exp []
	where
	aux (Value v) xs 	= (intToDigit (fromInteger v)) : xs
	aux (Add ex1 ex2) xs 	= aux ex1 ('+' : aux ex2 xs)
	aux (Diff ex1 ex2) xs 	= aux ex1 ('-' : aux ex2 xs)
	aux (Mult ex1 ex2) xs 	= aux ex1 ('*' : aux ex2 xs)

postOrder exp  = aux exp []
  where
  aux (Value v) xs 		= (intToDigit (fromInteger v)) : xs
  aux (Add ex1 ex2) xs 	= aux ex1 (aux ex2 ('+':xs))
  aux (Diff ex1 ex2) xs 	= aux ex1 (aux ex2 ('-':xs))
  aux (Mult ex1 ex2) xs 	= aux ex1 (aux ex2 ('*':xs))

foldExpr :: (Integer -> a) ->
 (a -> a -> a) ->
 (a -> a -> a) ->
 (a -> a -> a) ->
 Expr -> a
foldExpr ifValue ifAdd ifDiff ifMult e = fun e
 where
 fun (Value x) = ifValue x
 fun (Add e1 e2) = ifAdd (fun e1) (fun e2)
 fun (Diff e1 e2) = ifDiff (fun e1) (fun e2)
 fun (Mult e1 e2) = ifMult (fun e1) (fun e2)
 
evaluate' :: Expr -> Integer
evaluate' e = foldExpr id (+) (-) (*) e 