-- main :: IO()
-- main = putStr "Hello World"


--       /<<<<<<<<<<<<<<\left/<<<<<<<<<<<<<<\
--- [1,2,3].. , ([1,2,3],[1,2,3],[1,2,3]), [1,2,3]..
-- replace middle one with outer left.

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

        
-- tape = ([],repeat [1.])

-- main = do
--         let tape' = up tape        -- ([],[1..], [[1..],[1..]....])
--         let tape'' = up tape'      -- ([1..],[1..], [[1..],[1..]....])
--         let tape''' = up tape''     -- ([[1..].[1..], [1..], [[1..],[1..]....])
--         let tape''' = down tape''    -- ([],[1..], [[1..],[1..]....])
--         let tape'''' = down tape'''   -- ([],[], [[1..],[1..]....])

            