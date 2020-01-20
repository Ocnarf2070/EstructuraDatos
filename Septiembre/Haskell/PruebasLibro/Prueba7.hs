import Data.Char
import Text.PrettyPrint
miAccion :: IO()
miAccion = 
	do putStr "Dame un texto: "
	   xs <- getLine
	   putStr "En mayusculas es "
	   putStr (map toUpper xs)
	   putChar '\n'