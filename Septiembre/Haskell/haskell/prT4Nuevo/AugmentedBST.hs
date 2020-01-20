---------------------------------------------------------------------------------
-- ********************************************************
-- **** no olvide completar los datos de esta cabecera ****
-- ********************************************************

-- ETSI Informática. UMA
-- Estructuras de Datos. Ingeniería de _________________________ (2º _ ).
--
-- Práctica 6 - Ejercicio extra
--    Árboles binarios de búsqueda con información del peso en los nodos
--    (AUGMENTED Binary Search Trees, ABST)

-- Alumno: __________________________________

-- Fecha: _____________
-- Nº del ordenador del laboratorio: ____________
---------------------------------------------------------------------------------

module AugmentedBST
    where

import Prelude hiding (floor,ceiling)
import Data.Maybe(isJust)
import Data.List(nub,sort)
import DataStructures.Graphics.DrawTrees
import Test.QuickCheck

data ABST a = Empty | Node a Int (ABST a) (ABST a) deriving (Show,Eq)

weight Empty  = 0
weight (Node x w lt rt) = w

isABST :: (Ord a) => ABST a -> Bool
isABST Empty          =  True
isABST (Node x w lt rt) =   w == 1 + weight lt + weight rt
                         && keyOf lt  (<x)  && keyOf rt (>x)
                         && isABST lt && isABST rt
  where
    keyOf :: ABST a -> (a -> Bool) -> Bool
    keyOf Empty            p  =  True
    keyOf (Node y _ lt rt) p  =  p y

{-
Los ABST son una alternativa a los BST, en los cuales es posible implementar de forma
eficiente ciertas operaciones ya que en cada nodo aparece el peso total.
Entre las más interesantes figuran:

--  returns i-th smallest key in ABST (i=0 means returning the smallest value
--  in tree, i=1 the next one and so on).
select :: Int -> ABST a -> Maybe a

--  returns number of keys in ABST whose values are less than x.
rank :: Ord a => a -> ABST a -> Int

--  (rank inverse) returns number of keys in ABST whose values are greater than x.
rankInv :: Ord a => a -> ABST a -> Int

--  returns number of keys in ABST whose values are in range lo to hi.
sizeInIntervale :: Ord a => (a,a) -> ABST a -> Int

--  returns largest key in ABST whose value is less than or equal to k.
floor :: Ord a => a -> ABST a -> Maybe a

--  returns smallest key in ABST whose value is greater than or equal to k.
ceiling :: Ord a => a -> ABST a -> Maybe a

Por ejemplo, para el árbol t (definido en la forma t =  mkABST "dbmcnfta") tendríamos
el siguiente diálogo:

*AugmentedBST> pretty t
            ('d',8)_
           /        \
    ('b',3)         ('m',4)    <- desde el nodo 'm' cuelgan 4 claves
      / \             / \
('a',1) ('c',1) ('f',1) ('n',2)
                            \
                            ('t',1)

*AugmentedBST> select 5 t
Just 'm'

*AugmentedBST> rank 'g' t
5

*AugmentedBST> rankInv 'g' t
3

*AugmentedBST> sizeInIntervale ('d','m') t
3

*AugmentedBST> floor 'g' t
Just 'f'

*AugmentedBST> ceiling 'g' t
Just 'm'

*AugmentedBST> ceiling 'z' t
Nothing

Al ser BST aumentados, cualquier operación sobre éstos debe mantener el peso correcto en cada
nodo; así pues al eliminar una clave hay que reajustar algunos valores.
Por ejemplo, para la funciones que eliminan el mínimo  y una clave:

      deleteMin :: (Ord a) => ABST a -> ABST a
      delete    :: (Ord a) => a -> ABST a -> ABST a

tendríamos el siguiente diálogo:

*AugmentedBST> pretty t
            ('d',8)_
           /        \
    ('b',3)         ('m',4)
      / \             / \
('a',1) ('c',1) ('f',1) ('n',2)
                            \
                            ('t',1)

*AugmentedBST> pretty (deleteMin t)
        ('d',7)_
       /        \
('b',2)         ('m',4)
    \             / \
    ('c',1) ('f',1) ('n',2)
                        \
                        ('t',1)

*AugmentedBST> pretty (delete 'f' t)
            ('d',7)
           /    \
    ('b',3)     ('m',3)
      / \           \
('a',1) ('c',1)     ('n',2)
                        \
                        ('t',1)

-}


empty :: ABST a
empty =  Empty

isEmpty :: ABST a -> Bool
isEmpty Empty =  True
isEmpty _     =  False

---------------------------------------------------
--- ABST generation
---------------------------------------------------

mkABST :: (Ord a) => [a] -> ABST a
mkABST xs = foldl (flip insert) empty xs

{-
*AugmentedBST> pretty t
            ('d',8)_
           /        \
    ('b',3)         ('m',4)
      / \             / \
('a',1) ('c',1) ('f',1) ('n',2)
                            \
                            ('t',1)

-}
type T = Char
--  type T = Int
testMkABST = quickCheck (isABST . mkABST :: [T] -> Bool)

-------------------------------------------------------------------------------
-- Search
-------------------------------------------------------------------------------

isElem :: (Ord a) => a -> ABST a -> Bool
isElem x t = isJust (search x t)

search :: (Ord a) => a -> ABST a -> Maybe a
search x' Empty  = Nothing
search x' (Node x w lt rt)
  | x'<x         = search x' lt
  | x'>x         = search x' rt
  | otherwise    = Just x

minim :: ABST a -> a
minim Empty              = error "minim on empty tree"
minim (Node x _ Empty rt)  = x
minim (Node x _ lt rt)     = minim lt

maxim :: ABST a -> a
maxim Empty              = error "maxim on empty tree"
maxim (Node x _ lt Empty)  = x
maxim (Node x _ lt rt)     = maxim rt

-------------------------------------------------------------------------------
-- Insertion
-------------------------------------------------------------------------------

-- the function /node/ will be interesting to define delete and insert
node :: a -> ABST a -> ABST a -> ABST a
node x lt rt = Node x (1 + weight lt + weight rt) lt rt

insert :: (Ord a) => a -> ABST a -> ABST a
insert x' Empty  =  Node x' 1 Empty Empty
insert x' (Node x w lt rt)
    | x'<x       =  node x (insert x' lt) rt
    | x'>x       =  node x lt  (insert x' rt)
    | otherwise  =  Node x' w lt rt

-------------------------------------------------------------------------------
-- Deletion
-------------------------------------------------------------------------------

deleteMin :: (Ord a) => ABST a -> ABST a
deleteMin Empty  =  error "deleteMin sobre árbol vacío"
deleteMin (Node x w lt rt) = undefined

pDeleteMin t = not(isEmpty t) ==> not(isElem m t') && isABST t'
   where
        m  = minim t
        t' = deleteMin t

testDeleteMin = quickCheck (pDeleteMin :: ABST T -> Property)

delete :: (Ord a) => a -> ABST a -> ABST a
delete x' Empty  =  Empty
delete x' (Node x w lt rt)
  | x'<x       =  node x (delete x' lt) rt
  | x'>x       =  node x lt (delete x' rt)
  | otherwise  =  merge lt rt

merge :: ABST a -> ABST a -> ABST a
merge Empty rt     = rt
merge lt    Empty  = lt
merge lt    rt     = node x' lt rt'
  where (x',rt') = split rt

-- removes and returns minimum element from tree
split :: ABST a -> (a, ABST a)
split (Node x _ Empty rt)  = (x,rt)
split (Node x _ lt rt)  = (x', node x lt' rt)
  where (x',lt') = split lt

pDelete x t = not(isElem x t') && isABST t'
   where t' = delete x t

testDelete = quickCheck (pDelete :: T -> ABST T -> Bool)

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

inOrder :: ABST a -> [a]
inOrder t = aux t []
  where
    aux Empty          xs  = xs
    aux (Node x _ lt rt) xs  = aux lt (x : aux rt xs)

preOrder :: ABST a -> [a]
preOrder t  = aux t []
  where
    aux Empty          xs  = xs
    aux (Node x _ lt rt) xs  = x : aux lt (aux rt xs)

postOrder :: ABST a -> [a]
postOrder t  = aux t []
  where
    aux Empty          xs  = xs
    aux (Node x _ lt rt) xs  = aux lt (aux rt (x:xs))

-------------------------------------------------------------------------------
-- Generating arbirtray Binary Search Trees
-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (ABST a) where
  arbitrary = do
    xs <- arbitrary
    return (mkABST xs)

-------------------------------------------------------------------------------
-- Drawing a ABST
-------------------------------------------------------------------------------

instance Subtrees (ABST a) where
  subtrees Empty           = []
  subtrees (Node x w lt rt)  = [lt,rt]
  isEmptyTree Empty = True
  isEmptyTree _ = False

instance (Show a) => ShowNode (ABST a) where
  showNode (Node x w lt rt)  = show (x,w)

drawOnWith :: FilePath -> ((a,Int) -> String) -> ABST a -> IO ()
drawOnWith file toString = _drawOnWith file showBST
 where
  showBST (Node x w _ _) = toString (x,w)



{-
*****************************************************************************
El Ejercicio consiste en definir de forma EFICIENTE las siguientes funciones:
*****************************************************************************
  floor y rank
-}

--------------
-- Para probar
----------0 ----

ejBST =  mkABST "dbmcnftx"
ejBST2 = mkABST "fbmdnhtx"

-- ******************
--   floor
-- ******************

--  devuelve el mayor elemento del ABST  que es menor o igual a x.
floor :: Ord a => a -> ABST a -> Maybe a
floor x Empty    = Nothing
floor x (Node y w lt rt)
	| x == y = Just y
	| x < y = floor x lt
	| otherwise = Just $ case floor x rt of
        Nothing -> y
        Just rk -> rk

-- > floor 'f' ejBST
-- > Just 'f'
-- > floor 'h' ejBST
-- > Just 'f'
-- > floor 'a' ejBST
-- > Nothing

-----------------------------------------
--  tests for floor and ceiling
-----------------------------------------

-- de la lista /inOrder t/, seleccionamos el mayor de entre los menores o iguales a x
floor' x t = if null preFix then Nothing else Just (last preFix)
   where  preFix = takeWhile (<=x) (inOrder t)

equals f g = \x t -> f x t == g x t

testFloor   = quickCheck ( equals floor floor'  :: T -> ABST T -> Bool)

-- ******************
--   rank
-- ******************

--  returns number of keys in ABST whose values are less than x.
rank :: Ord a => a -> ABST a -> Int
rank x Empty    = 0
rank x (Node y w lt rt)
  | x == y       = weight lt
  | x < y        = rank x lt
  | otherwise    = 1 + weight lt + rank x rt

-- > rank 'a' ejBST
-- > 0
-- > rank 'b' ejBST
-- > 0
-- > rank 'e' ejBST
-- > 3