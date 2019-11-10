module Main where

import Lib

main :: IO()
main = do
        putStrLn "How many steps: (Integer)"
        input <- getLine
        let x = read input :: Integer

        putStrLn "X border left: (Integer)"
        input <- getLine
        let length_min = read input :: Integer

        putStrLn "X border right: (Integer)"
        input <- getLine
        let length_max = read input :: Integer

        putStrLn "Y border left: (Integer)"
        input <- getLine
        let width_min = read input :: Integer

        putStrLn "Y border Right: (Integer)"
        input <- getLine
        let width_max = read input :: Integer

        putStrLn "Do you want to show every step? (Bool)"
        input <- getLine
        let show_steps = read input :: Bool

        antMain x (length_min,length_max) (width_min,width_max) show_steps
       