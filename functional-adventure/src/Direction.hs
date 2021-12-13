module Direction where

-- Different Directions that the player can move in in the game
instance Show Direction where
    show N = "north"
    show S = "south"
    show E = "east"
    show W = "west"

data Direction = E | W | N | S deriving (Eq)