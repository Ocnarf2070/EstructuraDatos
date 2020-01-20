import DataStructures.Graph.DiGraph
import DataStructures.Graph.DiGraphBFT
import Data.List

mA=100
mB=49
data Jarras = J Integer Integer deriving (Eq,Ord,Show)

llenarA(J x y)=J mA y
llenarB(J x y)=J x mB
vaciarA(J x y)=J 0 y
vaciarB(J x y)=J x 0
volcarAenB(J x y)
	|cabeB>=x = J 0 (y+x)
	|otherwise = J (x-cabeB) mB
		where cabeB=mB-y
volcarBenA(J x y)
	|cabeA>=y = J (y+x) 0
	|otherwise = J mB (y-cabeA)
		where cabeA=mA-x
		
ops=[llenarA,llenarB,vaciarA,vaciarB,volcarAenB,volcarBenA]

sucJ j = [j'|op<-ops, let j'=op j, j/=j']

jarras=mkDiGraphSuc undefined sucJ

caminos= bftPaths jarras(J 0 0)

solucion=filter(\cs ->last cs==(J 0 1)) caminos

numeros=quickSort(nub [z | J x y <- bft jarras (J 0 0),z<-[x,y]])

quickSort::Ord a=>[a]->[a]
quickSort [] = []
quickSort (x:xs) = quickSort(menores) ++ [x] ++ quickSort(mayores)
	where
	menores = [y | y <-xs, y < x]
	mayores = [z | z <-xs, z >= x]