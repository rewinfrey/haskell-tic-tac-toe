module Game.Player where

data PlayerType  = Human | Computer deriving (Show, Eq)
data Player      = Player { token :: String, playerType :: PlayerType } deriving (Show, Eq)

newPlayerType :: String -> PlayerType
newPlayerType playerType = case playerType of "Human" -> Human
                                              "Computer" -> Computer
                                              _ -> Human

newPlayer :: String -> String -> Player
newPlayer token playerType = Player { token = token, playerType = newPlayerType playerType }
