import Test.QuickCheck

square :: Num a => a -> a
square x = x * x

p1 :: Integer -> Integer -> Property
p1 x y = True ==> square (x + y) == square x + square y + 2 * x * y

p2 :: Integer -> Integer -> Property
p2 x y = True ==> abs (x+y) == abs x +abs y

p3 :: Integer -> Integer -> Property
p3 x y = x >= 0 && y >= 0 ==> abs (x+y) == abs x + abs y