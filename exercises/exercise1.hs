guardTest :: Int -> String
guardTest value
    | value <= 10 = "onder 10"
    | value <= 20 = "onder 20"
    | value <= 30 = "onder 30"
    | otherwise   = "rest"



abcformula :: Int -> Int -> Int -> Int
abcformula a b c =
    let e = (b * b)
        f = (-4) * a * c
    in  (e - f)


    

describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "one long"  
          what xs = "longer then one"



maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)  


apply :: (a -> a) -> a -> a  
apply f x = f x
-- apply (*3) 10  // je kan als functie "*3" nemen


applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 

multTwo :: (Num a) => a -> a -> a
multTwo x y = x * y



zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
--  zipWith' (Function [+,-,*,/]) [list1] [list2]
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y  

map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map f xs  

