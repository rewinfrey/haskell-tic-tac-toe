module Console.Driver where

import System.Process
import Game.Player

x = "X"
o = "O"

human = "Human"
computer = "Computer"

start :: IO ()
start = do
  greeting
  player1TokenSelection <- getToken
  player1TypeSelection  <- getPlayer "1"
  player2TypeSelection  <- getPlayer "2"

  let player1Token = inputFromChoice player1TokenSelection x o
  let player1Type  = inputFromChoice player1TypeSelection human computer
  let player2Type  = inputFromChoice player2TypeSelection human computer

  putStrLn $ "\nPlayer 1's token is: " ++ player1Token
  putStrLn $ "\nPlayer 1 is: " ++ player1Type
  putStrLn $ "\nPlayer 2 is: " ++ player2Type

  let player1 = newPlayer player1Token player1Type
  let player2 = newPlayer (opposite player1Token) player2Type

  putStrLn $ "\nPlayer 1 real token is: " ++ token player1
  putStrLn $ "\nPlayer 2 real token is: " ++ token player2
  putStrLn "\nFinished!"

opposite :: String -> String
opposite token =
  case token of
    x -> o
    o -> x

greeting :: IO ()
greeting = putStrLn "\nWelcome to Haskell Tic-Tac-Toe!\n"

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
invalidInputPrompt = putStrLn "\nSorry, I dunno that!"

getToken :: IO String
getToken = do
  tokenPrompt
  choice <- getLine
  if inputValid choice then return choice else getToken

getPlayer :: String -> IO String
getPlayer playerNumber = do
  playerPrompt playerNumber
  choice <- getLine
  if inputValid choice then return choice else getPlayer playerNumber

inputValid :: String -> Bool
inputValid choice =
  case choice of
    "1" -> True
    "2" -> True
    _   -> False

inputFromChoice :: String -> String -> String -> String
inputFromChoice choice opt1 opt2 =
  case choice of
    "1" -> opt1
    "2" -> opt2
