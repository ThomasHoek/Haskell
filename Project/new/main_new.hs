import Debug.Trace

type GridInformation = (Integer, Integer , [(Integer,Integer)])

data Node a = Node
    { contents :: a
    , north    :: Node a
    , east     :: Node a
    , west     :: Node a
    , south    :: Node a
    , left     :: Node a
    , right    :: Node a
    }

grid :: (Integer -> Integer -> [(Integer,Integer)] -> a) -> Node a
grid f = origin 
    where
    origin = Node
        { contents = f 0 0 [(0,0)]             
        , north = growNorth 0 1 ([(0,0)] ++ [(0,1)]) origin
        , east = growEast  (-1) 0 ([(0,0)] ++ [(-1,0)]) origin
        , west = growWest  1 0 ([(0,0)] ++ [(1,0)]) origin
        , south = growSouth 0 (-1) ([(0,0)] ++ [(0,-1)]) origin
        , left = growEast  (-1) 0 ([(0,0)] ++ [(-1,0)]) origin
        , right = growWest  1 0 ([(0,0)] ++ [(1,0)]) origin
        }
    

    growNorth x y z s = self
        where
        self = Node
            { contents = f x y z
            ,north = growNorth x (y+1) (z ++ [(x,y+1)]) self
            ,east = growEast (x-1) y (z ++ [(x-1,y)]) self
            ,south = growSouth x (y-1) (z ++ [(x,y-1)]) self
            ,west = growWest (x+1) y (z ++ [(x+1,y)]) self
            ,left = growEast (x-1) y (z ++ [(x-1,y)]) self
            ,right = growWest (x+1) y (z ++ [(x+1,y)]) self
            }


    growEast x y z s = self
        where

        self = Node
            { contents = f x y z
            ,north = growNorth x (y+1) (z ++ [(x,y+1)]) self
            ,east = growEast (x-1) y (z ++ [(x-1,y)]) self
            ,south = growSouth x (y-1) (z ++ [(x,y-1)]) self
            ,west = growWest (x+1) y (z ++ [(x+1,y)]) self
            ,left = growSouth x (y-1) (z ++ [(x,y-1)]) self
            ,right = growNorth x (y+1) (z ++ [(x,y+1)]) self
            }


    growSouth x y z s = self
        where
        self = Node
            { contents = f x y z
            ,north = growNorth x (y+1) (z ++ [(x,y+1)]) self
            ,east = growEast (x-1) y (z ++ [(x-1,y)]) self
            ,south = growSouth x (y-1) (z ++ [(x,y-1)]) self
            ,west = growWest (x+1) y (z ++ [(x+1,y)]) self
            ,left = growEast (x-1) y (z ++ [(x-1,y)]) self
            ,right = growWest (x+1) y (z ++ [(x+1,y)]) self
            }
    

    growWest x y z s = self
        where
        self = Node
            { contents = f x y z
            ,north = growNorth x (y+1) (z ++ [(x,y+1)]) self
            ,east = growEast (x-1) y (z ++ [(x-1,y)]) self
            ,south = growSouth x (y-1) (z ++ [(x,y-1)]) self
            ,west = growWest (x+1) y (z ++ [(x+1,y)]) self
            ,left = growNorth x (y+1) (z ++ [(x,y+1)]) self
            ,right = growSouth x (y-1) (z ++ [(x,y-1)]) self
            }


-- coordsInList :: (Integer, Integer) -> [(Integer, Integer)]  -> Bool
-- coordsInList a b =  a `elem` b
            
showMainTest :: (Integer, Integer, [(Integer,Integer)]) -> [(Integer,Integer)]
showMainTest (a,b,c) = c
            



mainTest :: [(Integer,Integer)]
mainTest = do   
        let o = grid (\x y z -> trace ("compute " ++ show (x,y,z)) (x,y,z)) 

        let test = left . left. left. left $ o
        
        showMainTest(contents test)


main = print mainTest
       
        

        



-- http://haskell.1045720.n5.nabble.com/Infinite-grid-td3125863.html         
-- let o = grid (\x y -> trace ("compute " ++ show (x,y)) (x,y)) 
-- let o = grid (\x y z -> trace ("compute " ++ show (x,y,z)) (x,y,z)) 
-- let o = grid (\x y z -> trace (show(x,y,z)) (x,y,z)) 
-- contents o 
-- contents . north $  o 
-- contents . north . north . north . north . north $ o