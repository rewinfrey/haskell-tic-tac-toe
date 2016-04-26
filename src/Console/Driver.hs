module Console.Driver where

import System.Process

x = "X"
o = "O"

human = "Human"
computer = "Computer"

start :: IO ()
start = do
  greeting
  token <- getToken
  playerType1 <- getPlayer "1"
  playerType2 <- getPlayer "2"
  putStrLn $ "\nPlayer 1's token is: " ++ token
  putStrLn $ "\nPlayer 1 is: " ++ playerType1
  putStrLn $ "\nPlayer 2 is: " ++ playerType2

greeting :: IO ()
greeting = do
  -- createProcess (proc "clear" [])
  putStrLn "\nWelcome to Haskell Tic-Tac-Toe!\n"

tokenPrompt :: IO ()
tokenPrompt = do
  putStrLn "\nPlease choose player 1's token (choose 1 or 2):"
  putStrLn $ "1. " ++ x
  putStrLn $ "2. " ++ o

playerPrompt :: String -> IO ()
playerPrompt playerNumber = do
  putStrLn $ "\nPlease choose player " ++ playerNumber ++ " type (choose 1 or 2):"
  putStrLn $ "1. " ++ human
  putStrLn $ "2. " ++ computer

invalidInputPrompt :: IO ()
invalidInputPrompt = do
  putStrLn "\nSorry, I dunno that!"

getToken :: IO [Char]
getToken = do
  tokenPrompt
  choice <- getLine
  if (inputValid choice)
     then return $ inputFromChoice choice x o
     else do
       invalidInputPrompt
       getToken

getPlayer :: String -> IO [Char]
getPlayer playerNumber = do
  playerPrompt playerNumber
  choice <- getLine
  if (inputValid choice)
     then return $ inputFromChoice choice human computer
     else do
       invalidInputPrompt
       getPlayer playerNumber

inputValid :: [Char] -> Bool
inputValid choice =
  case choice of
    "1" -> True
    "2" -> True
    otherwise -> False

inputFromChoice :: [Char] -> String -> String -> String
inputFromChoice choice opt1 opt2 =
  case choice of
    "1" -> opt1
    "2" -> opt2
