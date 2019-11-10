import Debug.Trace

data Node a = Node -- ant can only move left and right
    { contents :: a
    , moveLeft     :: Node a
    , moveRight    :: Node a
    }

grid :: (Integer -> Integer -> Integer -> [(Integer,Integer)] -> a) -> Node a
grid f = origin 
    where
    origin = Node -- Ant will start going Right
        { contents = f 0 0 0 []                     
        , moveLeft = growEast 1 (-1) 0 [(0,0)] origin --go to east
        , moveRight = growWest 1 1 0 [(0,0)] origin  --go to west
        }
    

    growNorth c x y z s = self -- if now facing north
        where
        self = Node
            { contents = f c x y z
            ,moveLeft = growEast (c+1) (x-1) y (z ++ [(x,y)]) self --go to east
            ,moveRight = growWest (c+1) (x+1) y (z ++ [(x,y)]) self --go to west
            }


    growEast c x y z s = self --facing east
        where
        self = Node
            { contents = f c x y z
            ,moveLeft = growSouth (c+1) x (y-1) (z ++ [(x,y)]) self --go to south
            ,moveRight = growNorth (c+1) x (y+1) (z ++ [(x,y)]) self -- go to north
            }

    growSouth c x y z s = self --facing south
        where
        self = Node
            { contents = f c x y z
            ,moveLeft = growWest (c+1) (x+1) y (z ++ [(x,y)]) self --go to west
            ,moveRight = growEast (c+1) (x-1) y (z ++ [(x,y)]) self --go to east
            }
            
    growWest c x y z s = self --facing west
        where
        self = Node
            { contents = f c x y z
            ,moveLeft = growNorth (c+1) x (y+1) (z ++ [(x,y)]) self -- go to north
            ,moveRight = growSouth (c+1) x (y-1) (z ++ [(x,y)]) self -- go to south
            }


count   :: Eq a
        => a   -- ^ uitleg over variabele x
        -> [a] -- ^ uileg over lijst
        -> Int
count x =  length . filter (==x)


nextMoveAnt :: (Integer,Integer, Integer, [(Integer, Integer)]) -> Node a0 -> Node a0
nextmoveAnt (_,_,_,[]) = moveLeft
nextMoveAnt (_,x,y,z)
            | ((x,y) `count` z) `mod` 2 == 1 = moveRight 
            | otherwise = moveLeft
-- ^ Uitleg over nextMoveAnt

mainLoop :: Node (Integer,Integer, Integer, [(Integer, Integer)]) -> Integer -> Node (Integer,Integer, Integer, [(Integer, Integer)])
mainLoop a b 
            | b <= 0 = a   
            | otherwise = mainLoop ((nextMoveAnt . contents $ a) $ a) (b-1)

-- * Datatypes

-- | mainTest functie uitleg bla bla
mainTest :: Integer -> (Integer,Integer, Integer, [(Integer, Integer)])
mainTest a = do   
        let o = grid (\c x y z -> trace ("Step: " ++ show c ++ "       Position:  " ++ show(x,y)) (c,x,y,z))         
        let b = mainLoop o a
        contents b

main :: IO()
main = do
        putStrLn "How many steps: "
        input <- getLine
        let x = read input :: Integer
        print (mainTest x)
       
        

        



-- http://haskell.1045720.n5.nabble.com/Infinite-grid-td3125863.html         
-- let o = grid (\x y -> trace ("compute " ++ show (x,y)) (x,y)) 
-- let o = grid (\x y z -> trace ("compute " ++ show (x,y,z)) (x,y,z)) 
-- let o = grid (\x y z -> trace (show(x,y,z)) (x,y,z)) 
-- contents o 
-- contents . north $  o 
-- contents . north . north . north . north . north $ o