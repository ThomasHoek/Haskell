import Debug.Trace

type GridInformation = (Integer, Integer , [(Integer,Integer)])

data Node a = Node
    { contents :: a
    , moveLeft     :: Node a
    , moveRight    :: Node a
    }

grid :: (Integer -> Integer -> [(Integer,Integer)] -> a) -> Node a
grid f = origin 
    where
    origin = Node
        { contents = f 0 0 []                     
        , moveLeft = growEast  (-1) 0 [(0,0)] origin
        , moveRight = growWest  1 0 [(0,0)] origin
        }
    

    growNorth x y z s = self
        where
        self = Node
            { contents = f x y z
            ,moveLeft = growEast (x-1) y (z ++ [(x,y)]) self
            ,moveRight = growWest (x+1) y (z ++ [(x,y)]) self
            }


    growEast x y z s = self
        where
        self = Node
            { contents = f x y z
            ,moveLeft = growSouth x (y-1) (z ++ [(x,y)]) self
            ,moveRight = growNorth x (y+1) (z ++ [(x,y)]) self
            }

    growSouth x y z s = self
        where
        self = Node
            { contents = f x y z
            ,moveLeft = growWest (x+1) y (z ++ [(x,y)]) self
            ,moveRight = growEast (x-1) y (z ++ [(x,y)]) self
            }
            
    growWest x y z s = self
        where
        self = Node
            { contents = f x y z
            ,moveLeft = growNorth x (y+1) (z ++ [(x,y)]) self
            ,moveRight = growSouth x (y-1) (z ++ [(x,y)]) self
            }


-- coordsInList :: (Integer, Integer) -> [(Integer, Integer)]  -> Bool
-- coordsInList a b =  a `elem` b
     
count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)


nextMoveAnt :: (Integer, Integer, [(Integer, Integer)]) -> Node a0 -> Node a0
nextmoveAnt (_,_,[]) = moveLeft
nextMoveAnt (x,y,z)
            | ((x,y) `count` z) `mod` 2 == 1 = moveRight 
            | otherwise = moveLeft


mainLoop :: Node (Integer, Integer, [(Integer, Integer)]) -> Integer -> Node (Integer, Integer, [(Integer, Integer)])
mainLoop a b 
            | b <= 0 = a   
            | otherwise = mainLoop ((nextMoveAnt . contents $ a) $ a) (b-1)



mainTest :: Integer -> (Integer, Integer, [(Integer, Integer)])
mainTest a = do   
        let o = grid (\x y z -> trace ("compute " ++ show (x,y,z)) (x,y,z))         
        let b = mainLoop o a
        contents b


main = print (mainTest 1)
       
        

        



-- http://haskell.1045720.n5.nabble.com/Infinite-grid-td3125863.html         
-- let o = grid (\x y -> trace ("compute " ++ show (x,y)) (x,y)) 
-- let o = grid (\x y z -> trace ("compute " ++ show (x,y,z)) (x,y,z)) 
-- let o = grid (\x y z -> trace (show(x,y,z)) (x,y,z)) 
-- contents o 
-- contents . north $  o 
-- contents . north . north . north . north . north $ o