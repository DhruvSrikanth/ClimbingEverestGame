module GameState where

import Data.List
import Data.Char
import Control.Exception
import qualified Data.Map as M
import Data.Maybe

import Item
import Room
import Player
import Direction

type GameMap = M.Map RoomName Room

type Point = (Int, Int)
type GameGrid = M.Map Point Room

type Error a = Either String a

data GameState = GameState {
 message :: Maybe String
 , gmap :: GameMap
 , grid :: GameGrid
 , universe :: Universe
 , player :: Player
 }
 deriving (Show)

-- Make a GameMap from a list of rooms
mkMap :: [Room] -> GameMap
mkMap rooms = M.fromList (map makePair rooms)
  where makePair room = (rname room, room)

-- Create a gameGrid
gameGrid :: GameGrid
gameGrid = M.fromList $ Data.List.zip [(6,0), (5,0), (4,0), (4,1), (3,0), (2,0), (2,1), (1,0), (0,0)] allRooms 

-- Create a gameMap
gameMap :: GameMap
gameMap = mkMap allRooms

-- Create the initial state that the game will start in
initialState :: GameState
initialState = GameState Nothing gameMap gameGrid univ you

data KeyError = KeyError
  deriving Show

instance Exception KeyError

-- Get an object from the universe based on its name
getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

-- Get an object from the game state based on its name
getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

-- Get a room from the game map based on its name
getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

-- Get a room from the game state based on its name
getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

-- Create a new room in the game map in place of another room present
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap room_to_replace new_room game_map = mkMap updated_current_rooms
  where
    current_rooms = map (\room_name -> getRoomMap room_name game_map) Room.roomNames
    updated_current_rooms = (filter (\room_ -> (rname room_) /= room_to_replace) current_rooms) <> [new_room]


-- Set the message of the game state
setMessage :: String -> GameState -> GameState
setMessage message game_state = game_state{message = formatted_message}
  where
    formatted_message = if message == ""
                          then
                            Nothing
                        else
                          Just message

-- get the current inventory of the player
currentInventory :: GameState -> [ItemName]
currentInventory game_state = inventory $ player game_state

-- Get the current room that the player is in
currentRoom :: GameState -> Room
currentRoom game_state = getRoom player_room_name game_state
  where
    player_room_name = location $ player game_state

-- Get all nearby objects to the player (objects in the same room)
nearbyObjects :: GameState -> [ItemName]
nearbyObjects game_state = objects $ currentRoom game_state

-- Get the inventory weight of the player
inventoryWeight :: GameState -> Integer
inventoryWeight game_state = sum player_item_weights
  where
    players_items = currentInventory game_state
    player_item_weights = map (\(item_name, item) -> if (elem item_name players_items) then (weight item) else 0) $ M.toList univ

-- Checking if the player already has the item that they are trying to take
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck item_name game_state = if (elem item_name $ currentInventory game_state) 
                                              then 
                                                Left ("You are already carrying the " <> (show item_name) <> ".")
                                            else
                                              Right game_state

-- Check if the player is in the right room where they are trying to take an item
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck item_name game_state = if (elem item_name $ objects $ currentRoom game_state)
                                         then
                                            Right game_state
                                        else
                                          Left ("There is no " <> (show item_name) <> " in this room.")

-- Check if the player is holding too much in their inventory
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck item_name game_state = if (inventory_weight + item_weight) > player_max_weight
                                     then
                                        Left "That's too much weight for you to carry."
                                    else
                                      Right game_state
                                    where
                                      player_max_weight = maxWeight $ player game_state
                                      item_weight = weight $ getObject item_name game_state
                                      inventory_weight = inventoryWeight game_state

-- Check if the player is trying to drop an item that they dont have in their inventory
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck item_name game_state = if (elem item_name $ objects $ currentRoom game_state) || (elem item_name $ currentInventory game_state)
                                           then
                                              Right game_state
                                          else
                                            Left ("What do you mean, drop the " <> (show item_name) <> "?")

-- Check if the player is trying to drop an item in a room that already has that item (not possible in this games logic design)
inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck item_name game_state = if (elem item_name $ objects $ currentRoom game_state)
                                         then
                                           Left ("You aren't carrying the " <> (show item_name) <> ".")
                                       else
                                         Right game_state
                                          

-- Take an item
-- New version
-- In the takeItem and dropItem functions, I have considered the below order of checks to be the optimal one, however, in the exercise you mentioned that it matters. Is this the right order?
takeItem :: ItemName -> GameState -> GameState
takeItem item_name game_state = case (alreadyHaveTakeCheck item_name game_state) of
  Left error_message -> setMessage error_message game_state
  Right game_state -> case (inRoomTakeCheck item_name game_state) of
    Left error_message -> setMessage error_message game_state
    Right game_state -> case (weightCheck item_name game_state) of
      Left error_message -> setMessage error_message game_state
      Right game_state -> setMessage updated_message updated_game_state
        where
          updated_message = "You take the " <> (show item_name) <> "."
          updated_room = Room.removeItem item_name (currentRoom game_state)
          updated_game_map = setRoomMap (rname updated_room) updated_room (gmap game_state)
          updated_player = Player.addItem item_name (player game_state)
          updated_game_state =  game_state{gmap = updated_game_map, player = updated_player}

-- Drop an item
-- New version
dropItem :: ItemName -> GameState -> GameState
dropItem item_name game_state = case (anywhereDropCheck item_name game_state) of
  Left error_message -> setMessage error_message game_state
  Right game_state -> case (inRoomDropCheck item_name game_state) of
    Left error_message -> setMessage error_message game_state
    Right game_state -> setMessage updated_message updated_game_state
      where
        updated_message = "You drop the " <> (show item_name) <> "."
        updated_room = Room.addItem item_name (currentRoom game_state)
        updated_game_map = setRoomMap (rname updated_room) updated_room (gmap game_state)
        updated_player = Player.removeItem item_name (player game_state)
        updated_game_state =  game_state{gmap = updated_game_map, player = updated_player}


-- Check if the room has any objects present in it
roomHasObjects :: GameState -> Bool
roomHasObjects game_state = (length $ objects $ currentRoom game_state) /= 0

-- Get the name of the room in a particular direction from the current room
destinationName :: Direction -> Room -> Maybe RoomName
destinationName direction room = possible_destination
                                 where
                                   exits_ = exits room
                                   destinations = filter(\(dir, room_name) -> dir == direction) exits_
                                   possible_destination = if (length destinations) == 0
                                    then
                                      Nothing
                                    else
                                      Just $ snd $ head destinations

-- move from one room to another based on the direction
-- Only just realized I can update elements in a record this way (update = create new instance of the existing record). Makes it a lot easier!
move :: Direction -> GameState -> GameState
move direction game_state = case destinationName direction (currentRoom game_state) of
  Just room -> setMessage ("You go " <> show direction <> ".") game_state{player = Player.newLocation room (player game_state)}
  Nothing -> setMessage ("There is no exit in that direction.") game_state

-- Extra feature - Check whether the player is above 8000m
-- (at the South Summit, the Summit of Lhotse or the Summit of Everest)
-- and if they are, they must be carrying the oxygen and some snacks to survive
willDieGame :: GameState -> Bool
willDieGame game_state = if (player_location == SouthSummit || player_location == Lhotse || player_location == Everest)
                           then
                             not ((elem Oxygen $ inventory $ player game_state) && (elem Snacks $ inventory $ player game_state))
                         else
                           False
                         where
                           player_location = location $ player game_state

-- Check to see if the player is holding the flag at the summit of everest
-- to win the game
haveWonGame :: GameState -> Bool
haveWonGame game_state = (elem Flag $ inventory $ player game_state) && ((location $ player game_state) == Everest)

-- Display the exits in a particular direction of the room
displayExit :: Direction -> Room -> String
displayExit direction room = case (elem direction exit_directions) of
                               True -> case direction of
                                         N -> "|"
                                         S -> "|"
                                         W -> "-"
                                         E -> "-"
                               False -> " "
                             where
                               exit_directions = map fst (exits room)

-- Get the first letter of the room
roomLetter :: Maybe RoomName -> Room -> String
roomLetter maybe_room_name room = case maybe_room_name of
                                    Nothing -> (Data.Char.toUpper $ head $ take 1 $ (show . rname) room) : []
                                    Just room_name -> case (show room_name == (show . rname) room) of
                                                        True -> "*"
                                                        False -> (Data.Char.toUpper $ head $ take 1 $ (show . rname) room) : []


-- Get the coordinate mapping of the rooms based on the grid
mapCoords :: GameGrid -> [[(Int, Int)]]
mapCoords game_grid = [[(x, y) | y <- [y_min .. y_max]] | x <- [x_min .. x_max]]
                      where 
                        grid_coords = M.keys game_grid
                        x_min = minimum $ map fst grid_coords
                        x_max = maximum $ map fst grid_coords
                        y_min = minimum $ map snd grid_coords
                        y_max = maximum $ map snd grid_coords

-- Show the game grid as a list of lists of rooms
showGridList :: GameGrid -> [[Room]]
showGridList game_grid = [ Data.Maybe.catMaybes [ M.lookup coord game_grid | coord <- row ] | row <- mapCoords game_grid ]

-- Show the rows as a string
showRow :: Maybe RoomName -> [Room] -> String
showRow maybe_room_name rooms = intercalate "\n" [" " ++ north_exits, east_and_west_exits, " " ++ south_exits ++ "\n"]
  where
    north_exits = intercalate "  " $ map (displayExit N) rooms
    south_exits = intercalate "  " $ map (displayExit S) rooms
    east_and_west_exits = intercalate "" $ rooms >>= (\room -> [displayExit W room, roomLetter maybe_room_name room, displayExit E room])

-- Show the entire grid as a string
showGrid :: Maybe RoomName -> GameGrid -> String
showGrid maybe_room_name game_grid = intercalate "\n" $ map (showRow maybe_room_name) $ showGridList game_grid


