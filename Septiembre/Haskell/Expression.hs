module Expression (
	Item (..), Expression,value,showExpr,sample1,sample2) where
	
data Item = Add | Dif | Mul | Value Integer | LeftP | RightP deriving Show

type Expression = [Item]

sample1 = [Value 5, Add, LeftP , Value 6, Dif, Value 2, RightP, Mul, Value 3]
sample2 = [Value 5, Value 6, Value 2, Dif, Value 3, Mul, Add]

value :: Item -> Integer -> Integer -> Integer
value Add x y = x + y
value Dif x y = x - y
value Mul x y = x * y

showExpr :: Expression -> String
showExpr [] = ""
showExpr (Value x : ts) = ' ' : show x ++ showExpr ts
showExpr (Add : ts) = ' ' : '+' : showExpr ts
showExpr (Dif : ts) = ' ' : '-' : showExpr ts
showExpr (Mul : ts) = ' ' : '*' : showExpr ts
showExpr (LeftP : ts) = ' ' : '(' : showExpr ts
showExpr (RightP : ts) = ' ' : ')' : showExpr ts