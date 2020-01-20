
import Data.List(intercalate)
import DataStructures.Util.Random

dados :: Int -> Seed -> [Int]
dados n seed = take n (randomsR (1,6) seed)

sumDados :: Int -> Seed -> String
sumDados n seed = show n ++ " Dados: " ++ intercalate "," (aux list) ++ " Suma: " ++ show sumList
	where
	list = dados n seed
	sumList = sum list
	aux [] = []
	aux (x:xs) = show x : aux xs
	
	
