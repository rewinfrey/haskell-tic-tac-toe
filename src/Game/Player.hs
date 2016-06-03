module Game.Player where

data PlayerType  = Human | Computer deriving (Show, Eq)
data Player      = Player { token :: Token, playerType :: PlayerType } deriving (Show, Eq)
data Token       = X | O | Blank deriving (Show, Eq)

newPlayerType :: String -> PlayerType
newPlayerType playerType = case playerType of "Human" -> Human
                                              "Computer" -> Computer
                                              _ -> Human

newToken :: String -> Token
newToken token = case token of
                   "X" -> X
                   "O" -> O
                   _   -> X

newPlayer :: String -> String -> Player
newPlayer token playerType = Player { token = newToken token, playerType = newPlayerType playerType }
