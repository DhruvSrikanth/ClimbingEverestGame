module Room where


import Data.List as L

import Item
import Direction

-- Different rooms present in the game
instance Show RoomName where
   show BaseCamp = "base camp (17500 ft)"
   show Camp1 = "camp 1 (19500 ft)"
   show Camp2 = "camp 2 (21000 ft)"
   show Nuptse = "Nuptse summit (25790 ft)"
   show Camp3 = "camp 3 (23500 ft)"
   show ABC = "advanced base camp (26300 ft)"
   show Lhotse = "Lhotse summit (27940 ft)"
   show SouthSummit = "south route summit (28704 ft)"
   show Everest = "Everest summit (29035 ft)"


data RoomName
  = BaseCamp
  | Camp1
  | Camp2
  | Nuptse
  | Camp3
  | ABC
  | Lhotse
  | SouthSummit
  | Everest
  deriving (Eq, Ord)

type Exit = (Direction, RoomName)

data Room = Room {
 rname :: RoomName
 , desc :: String
 , exits :: [Exit]
 , objects :: [ItemName]
 }
 deriving (Show, Eq)

baseCamp :: Room
baseCamp = Room BaseCamp "Everest base camp...this is where it all begins!" [(N, Camp1)] [Ropes, Ladders]

camp1 :: Room
camp1 = Room Camp1 "You made it through the infamous and ever-changing, Khumbu icefall, all the way to camp 1" [(N, Camp2), (S, BaseCamp)] [TrekkingPoles]

camp2 :: Room
camp2 = Room Camp2 "You've got to camp 2! That rumbling you hear are the avalanches bellowing down the face of Nuptse" [(N, Camp3), (S, Camp1), (E, Nuptse)] [IceAxes]

nuptse :: Room
nuptse = Room Nuptse "Very impressive! You've summitted the legendary Mt. Nuptse!" [(W, Camp2)] [Camera]

camp3 :: Room
camp3 = Room Camp3 "You just traversed the magnificent Lhotse face and are now in camp 3!" [(N, ABC), (S, Camp2)] [Snacks]

abc :: Room
abc = Room ABC "You are at the divide between Mt. Everest and Mt. Lhotse, the 1st and 4th tallest mountains in the world! This is advanced base camp... but be careful, you are now entering the death zone where there is not enough oxygen for the body to sustain itself." [(N, SouthSummit), (S, Camp3), (E, Lhotse)] [Oxygen]

lhotse :: Room
lhotse = Room Lhotse "WOW! You've summitted the legendary Mt. Lhotse, the 4th tallest mountain in the world!" [(W, ABC)] [Skis]

southSummit :: Room
southSummit = Room SouthSummit "You are at the south summit...look around carefully for a flag left behind by your fellow countrymen. Your only challenge remaining is the almost vertical Hilary Step!" [(N, Everest), (S, ABC)] [Flag]

everest :: Room
everest = Room Everest "YOU DID IT! YOU SUMMITED MOUNT EVEREST, THE TALLEST FREE STANDING MOUNTAIN IN THE WORLD!!!" [(S, SouthSummit)] [SummitStone]

-- List of all the rooms present in the game
allRooms :: [Room]
allRooms = [baseCamp, camp1, camp2, nuptse, camp3, abc, lhotse, southSummit, everest]

-- List of room names present in the game
roomNames :: [RoomName]
roomNames = map (\room -> rname room) allRooms

-- Add an item to the room
addItem :: ItemName -> Room -> Room
addItem item_name room = Room room_name description exits_ objects_
 where
    room_name = rname room
    description = desc room
    exits_ = exits room
    objects_ = (objects room) ++ [item_name]

-- Remove an item from the room
removeItem :: ItemName -> Room -> Room
removeItem item_name room = Room room_name description exits_ objects_
 where
    room_name = rname room
    description = desc room
    exits_ = exits room
    objects_ = L.delete item_name (objects room)

-- Check if the room has objects
hasObjects :: Room -> Bool
hasObjects room = (length $ objects room) /= 0