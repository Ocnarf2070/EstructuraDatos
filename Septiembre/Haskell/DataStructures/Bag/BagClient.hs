-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º C. ETSI Informática. UMA
-- Práctica 3 - Uso del TAD Bag
--
-- Alumno: APELLIDOS, NOMBRE
--
-------------------------------------------------------------------------------

module BagClient where

import Data.Char(toLower)
import LinearBag

-- convertir una lista en una bolsa
--
-- BagClient> list2Bag "abracadabra"
-- LinearBag { 'a' 'a' 'a' 'a' 'a' 'b' 'b' 'c' 'd' 'r' 'r' }

list2Bag:: Ord a => [a] -> Bag a
list2Bag = foldr insert empty

-------------------------------------------------------------------------------
-- Uso del TAD Bag a través del plegado
-------------------------------------------------------------------------------

-- El TAD Bag es casi inútil sin una función de plegado. El plegado es un
-- iterador que permite recorrer una bolsa sin conocer su implementación.

-------------------------------------------------------------------------------
-- Ejercicios de plegados 
-------------------------------------------------------------------------------

-- convertir una bolsa en una lista de pares (a,Int)
--
-- BagClient> bag2List (list2Bag "abracadabra")
-- [('a',5),('b',2),('c',1),('d',1),('r',2)]

bag2List :: Ord a => Bag a -> [(a, Int)]
bag2List = foldBag f []
	where
		f x ox bg = (x,ox) : bg

-- Determina si contiene una bolsa a un elemento
--
-- BagClient> contains 'b' (list2Bag "abracadabra")
-- True

contains :: Ord a => a -> Bag a -> Bool
contains x = foldBag f False
	where
		f y oy bg = x == y || bg 

-- número de veces que aparece el elemento que aparece más veces en una bolsa
--
-- ClienteBolsa> maxOcurrences (list2Bag "abracadabra")
-- 5

maxOcurrences :: Ord a => Bag a -> Int
maxOcurrences = foldBag f 0
	where
		f y oy bg = max oy bg
