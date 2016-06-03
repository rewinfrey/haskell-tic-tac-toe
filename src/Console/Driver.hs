module Console.Driver where

import System.Process
import Game.AIPlayer ( minimax, MinMaxPlayer(..) )
import Game.Board ( update, boardState, State(..), Board, Space(..), Location )
import Game.Context
import Game.Player ( Token(..), newPlayer, Player(..), PlayerType(..) )
import Data.Foldable (maximumBy)
import Data.List (intercalate)
import Data.Matrix (toLists)

x = "X"
o = "O"

defaultBoardSize = 3

human = "Human"
computer = "Computer"

start :: IO ()
start = do
  greeting
  gameContext <- setupGame
  playGame gameContext

opposite :: String -> String
opposite x = o
opposite o = x

greeting :: IO ()
greeting = putStrLn "\nWelcome to Haskell Tic-Tac-Toe!\n"

setupGame :: IO GameContext
setupGame = do
  player1TokenSelection <- getToken
  player1TypeSelection  <- getPlayer "1"
  player2TypeSelection  <- getPlayer "2"

  player1Token <- inputFromChoice player1TokenSelection x o
  player1Type  <- inputFromChoice player1TypeSelection human computer
  player2Type  <- inputFromChoice player2TypeSelection human computer

  let player1 = newPlayer player1Token player1Type
  let player2 = newPlayer (opposite player1Token) player2Type

  return $ newGameContext player1 player2 defaultBoardSize

playGame :: GameContext -> IO ()
playGame gameContext@ GameContext { board = board, player1 = player1, player2 = player2, currentPlayer = currentPlayer } = do
  displayBoard board
  move <- if playerType currentPlayer == Human then humanPlayerTurn else computerPlayerTurn currentPlayer board
  let updatedBoard = update board move (Game.Player.token currentPlayer)
  let nextPlayer = if currentPlayer == player1 then player2 else player1
  case boardState updatedBoard of
    Winner    -> displayWinner updatedBoard currentPlayer
    Tie       -> displayTie updatedBoard
    Undecided -> playGame (gameContext { board = updatedBoard, currentPlayer = nextPlayer })

displayWinner :: Board -> Player -> IO ()
displayWinner board currentPlayer = do
  displayBoard board
  putStrLn $ show (Game.Player.token currentPlayer) ++ " is the winner!"

displayTie :: Board -> IO ()
displayTie board = do
  displayBoard board
  putStrLn "It's a tie!"

humanPlayerTurn :: IO Location
humanPlayerTurn = do
    humanMovePrompt
    move <- getLine
    return (read move :: Location)

computerPlayerTurn :: Player -> Board -> IO Location
computerPlayerTurn player board = do
  computerMovePrompt
  return (minimax board (token player))

humanMovePrompt :: IO ()
humanMovePrompt =
  putStrLn "Please choose a move location (e.g. (1,1))"

computerMovePrompt :: IO ()
computerMovePrompt =
  putStrLn "Computer is thinking..."

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

inputValid :: String -> Bool
inputValid choice =
  case choice of
    "1" -> True
    "2" -> True
    _   -> False

inputFromChoice :: String -> String -> String -> IO String
inputFromChoice choice opt1 opt2 =
  case choice of
    "1" -> return opt1
    "2" -> return opt2

displayBoard :: Board -> IO ()
displayBoard board = do
  let stringRows = rowToString <$> Data.Matrix.toLists board
  let maxLength = length $ Data.Foldable.maximumBy (\row1 row2 -> compare (length row1) (length row2)) stringRows
  let margin = "\n\n"
  let divider = "\n" ++ (concat $ replicate maxLength "-") ++ "\n"
  putStrLn $ margin ++ (Data.List.intercalate divider stringRows) ++ margin

rowToString :: [Space] -> String
rowToString spaces = Data.List.intercalate " | " $ spaceToString <$> spaces
  where spaceToString Space { location = location, spaceToken = spaceToken } = case spaceToken of
                                                                             Blank -> show location
                                                                             X -> "  X  "
                                                                             O -> "  O  "
