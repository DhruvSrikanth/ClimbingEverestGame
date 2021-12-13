module Player where

import Data.List as L

import Item
import Room

data Player = Player {
   inventory  :: [ItemName]
   , maxWeight  :: Integer
   , location :: RoomName
   }
   deriving (Show, Eq)

-- Add an item to a player's inventory
addItem :: ItemName -> Player -> Player
addItem item_name gamer = Player inv max_weight loc
 where
  inv = (inventory gamer) ++ [item_name]
  max_weight = maxWeight gamer
  loc = location gamer

-- Added two ways to remove the item in the inventory of a player for my own understanding
-- Remove an item from the player's inventory
removeItem :: ItemName -> Player -> Player
removeItem item_name gamer = Player inv max_weight loc
 where
  -- inv = filter (\x -> x /= item_name) (inventory gamer)
  inv = L.delete item_name (inventory gamer) 
  max_weight = maxWeight gamer
  loc = location gamer

-- Move the player to a new location
newLocation :: RoomName -> Player -> Player
newLocation room_name gamer = Player inv max_weight loc
 where
  inv = inventory gamer
  max_weight = maxWeight gamer
  loc = room_name

-- Check if the player is carrying anything
isCarryingAnything :: Player -> Bool
isCarryingAnything gamer = 
 if inventory gamer == []
   then False
 else
   True

-- The initial player
you :: Player
you = Player [] 160 BaseCamp