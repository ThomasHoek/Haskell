module Main where

import Lib

type PositionTuple = (Integer, Integer)

main :: IO()
main = do
        putStrLn "How many steps:"
        input <- getLine
        let amountOfSteps = read input :: Integer

        putStrLn "X border left:"
        input <- getLine
        let length_min = read input :: Integer
        
        putStrLn "X border right:"
        input <- getLine
        let length_max = read input :: Integer

        putStrLn "Y border Top:"
        input <- getLine 
        let width_min = read input :: Integer

        putStrLn "Y border Bottom:"
        input <- getLine
        let width_max = read input :: Integer
        
        putStrLn "Do you want to show every step?"
        input <- getLine
        let show_steps = read input :: Bool
        
        

        maincheck amountOfSteps (length_min,length_max) (width_min,width_max) show_steps 

maincheck :: Integer -> PositionTuple -> PositionTuple -> Bool -> IO()
maincheck amountOfSteps (length_min,length_max) (width_min,width_max) show_steps
        | amountOfSteps < 0 = print "Amount of steps is smaller then 0"
        | length_min > length_max =  print "Length min is bigger then Length max."
        | width_min > width_max =  print "Width min is bigger then Width max."
        | otherwise = antMain amountOfSteps (length_min,length_max) (width_min,width_max) show_steps
        