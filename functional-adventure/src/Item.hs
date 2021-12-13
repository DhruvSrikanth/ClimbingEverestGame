module Item where

import qualified Data.Map as M

-- Items I have chosen
instance Show ItemName where
  show SummitStone = "stone"
  show TrekkingPoles = "poles"
  show Skis = "skis"
  show IceAxes = "axes"
  show Ropes = "ropes"
  show Flag = "flag"
  show Snacks = "snacks"
  show Ladders = "ladders"
  show Oxygen = "oxygen"
  show Camera = "camera"


data ItemName
  = SummitStone
  | TrekkingPoles
  | Skis
  | IceAxes
  | Ropes
  | Flag
  | Snacks
  | Ladders
  | Oxygen
  | Camera
  deriving (Eq, Ord)


type Universe = M.Map ItemName Item

data Item = Item {
 iname  :: ItemName
 , weight  :: Integer
 }
 deriving (Show, Eq)

-- Create the universe from a list of items
mkUniverse :: [Item] -> Universe
mkUniverse items = M.fromList (map makePair items)
  where makePair item = (iname item, item)


summitStone :: Item
summitStone = Item SummitStone 20

trekkingPoles :: Item
trekkingPoles = Item TrekkingPoles 15

skis :: Item
skis = Item Skis 25

iceAxes :: Item
iceAxes = Item IceAxes 60

ropes :: Item
ropes = Item Ropes 80

flag :: Item
flag = Item Flag 1

snacks :: Item
snacks = Item Snacks 5

ladders :: Item
ladders = Item Ladders 75

oxygen :: Item
oxygen = Item Oxygen 80

camera :: Item
camera = Item Camera 10

-- item universe
univ = mkUniverse [summitStone, trekkingPoles, skis, iceAxes, ropes, flag, snacks, ladders, oxygen, camera]

-- Get a list of item names
itemNames :: [ItemName]
itemNames = map (\(item_name, item) -> item_name) $ M.toList univ