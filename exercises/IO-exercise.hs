import System.IO  
import System.Directory  
import Data.List

fileread = do   readfilef <- readFile "test.txt"
                putStrLn readfilef

inputreturn = do    putStr "Type " 
                    putStr "Input: "
                    getinput <- getLine
                    putStrLn $ "De input was: " ++ getinput


statetest :: Double -> Double -> (Double, Double)
statetest invoer state =
                let resultaat = invoer * 2
                in (resultaat, state + resultaat)



totaalstate :: Double -> (Double, Double)
totaalstate s = let (r, s') = statetest 2 s
                    (r', s'') = statetest 4 s'
                in statetest 2 s

