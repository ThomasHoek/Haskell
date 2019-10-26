map_plus :: Int -> Int
map_plus x = x + 3

function_map :: (a -> a) -> [a] -> [a]
function_map f [] = []
function_map f (x:xs) = f x : function_map f xs


-- show_negate :: Int -> String
show_negate = (show . map_plus  . negate)
show_negate2 = (show . negate . map_plus)



zip_function :: (Int -> Int -> Int)
zip_function x y = x + y 


zipme :: (a -> a -> a) -> [a] -> [a] -> [a]
zipme f [] [] = []
zipme f [] _ = []
zipme f _ [] = []
zipme f (x:xs) (y:ys) = f x y : zipme f xs ys


som_fold :: (Int -> Int -> Int)
som_fold x y = x + y

product_fold :: (Int -> Int -> Int)
product_fold x y = x * y

fold :: (a -> a -> a) -> [a] -> a
fold _ [x] = x
fold f (x:xs) = f x (fold f xs)


-- ul :: Int -> Tijdstip -> Tijdstip
-- ul = urenLater
-- fold_test = ul (Tijd 12 0) [3,2,5]

fold_test :: (b -> a -> a) -> a -> [b] -> a
fold_test f x (y:ys) = f y ( fold_test f x ys)


