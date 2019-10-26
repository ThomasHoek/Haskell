data Shape = Circle Float Float Float | Rectangle Float Float Float Float

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r^2
surface (Rectangle x1 x2 y1 y2) = (abs $ x2 - x1 ) * (abs $ y2 - y1 )

-- surface $ Circle 10 20 10  

-- surface $ Rectangle 0 0 100 100  


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right  


    singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
    

treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)      


class Functortest f where  
    fmaptest :: (a -> b) -> f a -> f b  


instance Functortest [] where
    fmaptest = map 

instance Functortest Maybe where  
    fmaptest f (Just x) = Just (f x)  
    fmaptest f Nothing = Nothing

instance Functortest Tree where  
    fmaptest f EmptyTree = EmptyTree  
    fmaptest f (Node x leftsub rightsub) = Node (f x) (fmaptest f leftsub) (fmaptest f rightsub)  