f :: Int -> Int
f x =  (x * x + x)

g :: Int -> String
g = show . f

h :: Int -> Char
h = head . g


data MaybeInt = Intcheck Int | Niets
misschien' :: MaybeInt -> Int
misschien' (Intcheck n) = n
misschien' Niets = 0



list_length :: [Char] -> Int
list_length x = length x

