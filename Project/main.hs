type Cell = [Bool]
type Grid = (Cell ,Cell , Cell ,Cell , Cell , Cell )

data LastMove = MoveLeft | MoveDown | MoveRight | MoveUp deriving(Show, Eq, Ord)

type Move = (LastMove, Cell)


anticlockwise :: LastMove -> LastMove
anticlockwise a
        | a == MoveLeft = MoveUp
        | a == MoveDown = MoveLeft
        | a == MoveRight = MoveDown
        | a == MoveUp  = MoveRight
        | otherwise = MoveUp

clockwise :: LastMove -> LastMove
clockwise a
        | a == MoveLeft = MoveDown
        | a == MoveDown = MoveRight
        | a == MoveRight = MoveUp
        | a == MoveUp  = MoveLeft
        | otherwise = MoveUp


gameStatus :: Move -> LastMove
gameStatus (a,b) 
        | head b = clockwise a
        | otherwise = anticlockwise a

move :: Move
move = (MoveLeft, [True])
        
move2 :: Move
move2 = (MoveLeft, [False])
        
        
 --  add state: state up -> do left change state to left
          -- state left -> do down change state to down
         -- state down -> do right change state to right
         -- state right -> do right change state to up

        --  state turn clockwise if false,false
        -- state turn anti clockwise if true,true       




up :: Grid -> Grid
up (a, b, c:cx, d, e, f)
            | not $ head b =  (a ++ [True], [c], cx,d,e,f)
            | otherwise =  (a ++ [False], [c], cx,d,e,f)


down :: Grid -> Grid
down (a,b,c,d,e,f)
            | not $ head b = (init a, [last a], True : c,d,e,f)
            | otherwise = (init a, [last a], False : c,d,e,f)


left :: Grid -> Grid
left (a, b, c, d, e,f:fx)
            | not $ head d =  (a,b,c,d ++ [True], [f], fx)
            | otherwise =  (a,b,c,d ++ [False], [f], fx)

right :: Grid  -> Grid
right (a,b,c,d,e,f)
            | not $ head e = (a,b,c,init d, [last d], True : f)
            | otherwise = (a,b,c,init d, [last d], False : f)
            

            

grid :: Grid
grid = ([False,False,False,False,False], -- x left
        [False],                         -- x curr
        [False,False,False,False,False], -- x right

        [False,False,False,False,False], -- y left
        [False],                         -- y curr
        [False,False,False,False,False]) -- y right



-- grid = 
--    ([False,False ..],                -- x left
--     [False],                         -- x curr
--     [False,False .. ],               -- x right
    
--     [False,False ..],                -- y left
--     [False],                         -- y curr
--     [False,False..])                 -- y right



-- if false,false -> change both to true and do clockwise
-- if false,true -> change the false to true and do clockwise
-- if true,false -> change the false to true and do clockwise
-- if true, true -> change the true which got changed to false and do anti clockwise        
        --      ||
        --     \||/
        --      \/
--  left (true true) -> up (true,false)
--  the x-axis does not change. Thus we only change the y-axis.

        