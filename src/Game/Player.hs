module Game.Player where

data PlayerType  = Human | Computer deriving (Show, Eq)
data Player      = Player { token :: Char, playerType :: PlayerType } deriving (Show, Eq)

newPlayerType :: String -> PlayerType
newPlayerType playerType = case playerType of "human" -> Human
                                              "computer" -> Computer
                                              _ -> Human

newPlayer :: Char -> PlayerType -> Player
newPlayer token playerType = Player { token = token, playerType = playerType }

