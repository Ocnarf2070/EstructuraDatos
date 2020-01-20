import DataStructures.Stack.LinearStack

s1 :: Stack Int
s1 = push 3 (push 2 (push 1 empty))

size :: Stack a -> Int
size s 
	|isEmpty s = 0
	|otherwise = 1 + size (pop s)
	
s2 :: Stack Int
s2 = push (size s1) s1