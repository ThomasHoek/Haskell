-- ['a','e','o','u','i']
import Data.Char(toLower)


disemvowel :: String -> String
disemvowel [] = []
disemvowel (x:xs)
                | (toLower x) `elem` ['a','e','o','u','i'] =  disemvowel xs
                | otherwise = x: disemvowel xs
                



