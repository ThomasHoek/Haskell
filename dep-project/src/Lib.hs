module Lib where

    import Debug.Trace
    import Data.List (nubBy)
    import Text.Show.Unicode
    import Data.List.Split

    type XYLocation = (Integer, Integer)
    -- ^Type used for xpos and ypos from the same state.

    type Grid = (Integer,Integer,Integer , [XYLocation])
    -- ^Grid which is used to store counter, xpos, ypos and list of xpos and ypos.

    
    data Node a = Node -- Ant can only move left and right
        { contents :: a
        -- ^Shows a
        , moveLeft     :: Node a
        -- ^Tells the grid that the next state should be moveLeft
        , moveRight    :: Node a
        -- ^Tells the grid that the next state should be moveRight
        }


    -- | if a tuple in the list is seen an even amount of times, remove both out the list.
    removeDuplTuples :: Eq a => [(a,a)] -> [(a,a)]
    removeDuplTuples x = [a | a <- x,  ((a `count` x) `mod` 2 /= 0)]
    


    {-| 
    State that changes depending on what data Node says. It has 5 states

    * origin: start

    > grid :: 0 0 0 []

    * moveNorth:

        > grid :: 1 1 0 [(0,0)]

            * Left -> moveEast

                > grid :: 1 1 -1 [(0,0),(1,0)]

            * Right -> moveWest

                > grid :: 1 1 1 [(0,0),(1,0)]

    * moveEast

        > grid :: 1 0 1 [(0,0))]

            * Left -> moveSouth

                > grid :: 1 -1 1 [(0,0),(0,1)]

            * Right -> moveNorth

                > grid :: 1 1 1 [(0,0),(0,1)]

    * moveSouth

        > grid :: 1 -1 0 [(0,0)]

            * Left -> moveWest

                > grid :: 1 -1 1 [(0,0),(-1,0)]

            * Right -> moveEast

                > grid :: 1 -1 -1 [(0,0),(-1,0)]

    * moveWest

        > grid :: 1 0 -1 [(0,0)]

            * Left -> moveNorth

                > grid :: 1 1 -1 [(0,0),(0,-1)]

            * Right -> moveSouth

                > grid :: 1 -1 -1 [(0,0),(0,-1)]

    -}           
    grid :: (Integer -> Integer -> Integer -> [XYLocation] -> a) -- ^(Counter -> Current Xpos -> Current Ypos -> [list of previous xpos & ypos] ->self)
             -> Node a -- ^This entire tuple gets returend as a state Node a
    grid f = origin 
        where
        origin = Node 
            { contents = f 0 0 0 []                     
            , moveLeft = moveEast 1 (-1) 0 [(0,0)] origin
            , moveRight = moveWest 1 1 0 [(0,0)] origin 
            }
        
        
        moveNorth c x y z s = self
            where
            self = Node
                { contents = f c x y z
                ,moveLeft = moveEast (c+1) (x-1) y (z ++ [(x,y)]) self                 
                ,moveRight = moveWest (c+1) (x+1) y (removeDuplTuples z ++ [(x,y)]) self 
                }
    
        
        moveEast c x y z s = self 
            where
            self = Node
                { contents = f c x y z
                ,moveLeft = moveSouth (c+1) x (y-1) (z ++ [(x,y)]) self             
                ,moveRight = moveNorth (c+1) x (y+1) (removeDuplTuples z ++ [(x,y)]) self 
                }
        
        moveSouth c x y z s = self
            where
            self = Node
                { contents = f c x y z
                ,moveLeft = moveWest (c+1) (x+1) y ( z ++ [(x,y)]) self                
                ,moveRight = moveEast (c+1) (x-1) y (removeDuplTuples z ++ [(x,y)]) self         
                }
                
        
        moveWest c x y z s = self 
            where
            self = Node
                { contents = f c x y z
                ,moveLeft = moveNorth (c+1) x (y+1) ( z ++ [(x,y)]) self
                ,moveRight = moveSouth (c+1) x (y-1) (removeDuplTuples z ++ [(x,y)]) self 
                }
    
    -- | Counts how many times a value is in the list.
    count   :: Eq a => a -> [a] -> Int
    count x =  length . filter (==x)
    
    -- | If the current position is already in the list give moveRight back else moveLeft
    nextMoveAnt :: Grid -> Node a0 -> Node a0
    nextMoveAnt (_,x,y,z) 
                | ((x,y) `count` z) `mod` 2 == 1 = moveRight 
                | otherwise = moveLeft
    
    
    -- | Takes 2 tuples and sets those as display grid. Then fills every value in the grid if its the current position or in the list of positions.
    displayMoves :: XYLocation -> XYLocation -> Grid -> String
    displayMoves (lengthMin,lengthMax) (widthMin,widthMax) (_ , xPos, yPos , xyList) = do
            let gridDisplay = [(j,i) | i <- [lengthMin..lengthMax], j <- [widthMin..widthMax]]
            let test = [if x == (xPos, yPos) then "[A]" else if x `notElem` xyList then "[ ]" else "[X]" | x <- gridDisplay]            
            let difference = (lengthMax - lengthMin) + 1
            let bigGrid = chunksOf (fromIntegral difference) test            
            concat . concat $ map (++["\n"]) bigGrid


    -- | The Integer is a counter. The function calls itself the counter amount of times.
    -- Every time its called it will make the node go 1 step further. 
    -- The Node will be decided using nextMoveAnt.
    antLoop :: Node Grid -> Integer -> Node Grid
    antLoop a b
                | b <= 0 = a   -- counter
                | otherwise = antLoop ((nextMoveAnt . contents $ a) a) (b-1) -- first it looks if it should move left or right, then does Recurrsion with the left/right step added.
        

    -- | This funtion takes an Integer, two XYlocation types and an bool. 
    -- Depending on the bool it will or wont show all the plots.
    langtonsant :: Integer  -- ^Counter of the steps taken.
                -> XYLocation -- ^Left and bottom border.
                -> XYLocation -- ^Top and right border.
                -> Bool -- ^Show all steps.
                -> Grid-- ^A grid which gets returned.

    langtonsant a b c d = do   
            let length = c 
            let width = b
            if d 
                then contents $ antLoop (grid (\c x y z -> trace(displayMoves length width (c,x,y,z)++ "\n\n\n\n" ) (c,x,y,z))) a
                else contents $ antLoop (grid (\c x y z -> trace(show c) (c,x,y,z))) a
                
                    
    -- | Sends all the inputs from main to langtonsant               
    antMain :: Integer -> XYLocation -> XYLocation -> Bool -> IO()
    antMain a b c d = putStrLn $ displayMoves b c $ langtonsant a b c d