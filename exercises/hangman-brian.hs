module Hangman where

import Control.Monad.Trans       (lift)
import Control.Monad.Trans.State (StateT, runStateT, modify, get)
import Data.Bifunctor            (first, second)
import Data.Either               (lefts, isRight)
import Data.List                 (intersperse)

type Letter  = Char
type TheWord = [Either Letter Letter]
type Guesses = [Letter]
type GameState = (TheWord, Guesses)

data Status = Win | Lose | Continue

inputToGuess :: String -> Letter
inputToGuess []    = ' '
inputToGuess (x:_) = x

type GameMonad = StateT GameState IO

game :: GameMonad ()               -- State/IO actie met return ()
game = do gs@(word, _) <- get
          lift $ printGame gs      -- liftIO
          case gameStatus gs of
            Win      -> lift $ putStrLn "You Win!"
            Lose     -> lift $ putStrLn "You Hang!"
            Continue -> do input <- inputToGuess <$> lift getLine
                           modify $ handleGuess input word
                           game

runGame :: String -> IO ()
runGame word = fst <$> runStateT game (map Left word, mempty)

main :: IO ()
main = runGame "haskell"

handleGuess :: Letter -> TheWord -> (GameState -> GameState)
handleGuess guess word | guess `elem` lefts word = first flipLetter
                       | otherwise               = second (guess :) 
  where flipLetter :: TheWord -> TheWord
        flipLetter = map (\cur -> if cur == (Left guess)     -- guess in scope door
                                          then Right guess   --   nested functie
                                          else cur)

printGame :: GameState -> IO ()
printGame (word, guess) = do putStr $ showWord $ word
                             putStr $ "    "
                             putStr $ show . length $ guess
                             putStrLn $ "/8"
  where showWord :: TheWord -> String
        showWord = intersperse ' ' . map (either (const '_') id)

gameStatus :: GameState -> Status
gameStatus (word, guess) | all isRight word  = Win
                         | length guess >= 8 = Lose
                         | otherwise         = Continue