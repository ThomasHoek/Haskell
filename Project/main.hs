type Cell a = [Bool]
type Grid a = (Cell a,Cell a, Cell a,Cell a, Cell a, Cell a)

up :: Grid a -> Grid a
up (a, b, c:cx, d, e, f)
            | not $ head b =  (a ++ [True], [c], cx,d,e,f)
            | otherwise =  (a ++ [False], [c], cx,d,e,f)


down :: Grid a -> Grid a
down (a,b,c,d,e,f)
            | not $ head b = (init a, [last a], True : c,d,e,f)
            | otherwise = (init a, [last a], False : c,d,e,f)


left :: Grid a -> Grid a
left (a, b, c, d, e,f:fx)
            | not $ head d =  (a,b,c,d ++ [True], [f], fx)
            | otherwise =  (a,b,c,d ++ [False], [f], fx)

right :: Grid a -> Grid a
right (a,b,c,d,e,f)
            | not $ head e = (a,b,c,init d, [last d], True : f)
            | otherwise = (a,b,c,init d, [last d], False : f)
            

grid :: Grid [Bool]
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

--  add state: state up -> do left change state to left
          -- state left -> do down change state to down
         -- state down -> do right change state to right
         -- state right -> do right change state to up

        --  state turn clockwise if false,false
        -- state turn anti clockwise if true,true

-- if false,false -> change both to true and do clockwise
-- if false,true -> change the false to true and do clockwise
-- if true,false -> change the false to true and do clockwise
-- if true, true -> change the true which got changed to false and do anti clockwise        
        --      ||
        --     \||/
        --      \/
--  left (true true) -> up (true,false)
--  the x-axis does not change. Thus we only change the y-axis.

        