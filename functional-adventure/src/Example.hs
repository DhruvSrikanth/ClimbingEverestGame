module Example where

import Data.List as L
import System.Random
import Data.Array.IO
import Control.Monad
import qualified Data.Map as M
import Data.Maybe

import Item
import Direction
import Room
import Player
import GameState

newtype Set = Set [Point] deriving (Eq, Show)

-- Make a set
mkSet :: (Int, Int) -> Set
mkSet (x,y) = Set [(x,y)]

-- Remove all exits from a room
zeroExits :: Room -> Room
zeroExits room = room {exits = []}

-- Get the coordinates of a 3x3 grid
-- Realized how important apply - <*> can be after the midterm
allCoords3x3 :: [Point]
allCoords3x3 = [(,)] <*> [0..size] <*> [0..size]
               where
                 size = 3

-- Create an example grid
exampleGrid :: [Room] -> IO GameGrid
exampleGrid rooms = do
                      shuffled_rooms <- shuffle rooms
                      return (M.fromList $ L.zip allCoords3x3 shuffled_rooms)

-- Check if two points are neigbhours
-- Wow pattern destructing is helpful...saved me about 5 lines of code
neighbors :: Point -> Point -> Bool
neighbors (x1, y1) (x2, y2) = (y1 == y2 && x1 == x2 + 1) 
                           || (y1 == y2 && x1 == x2 - 1)
                           || (x1 == x2 && y1 == y2 + 1)
                           || (x1 == x2 && y1 == y2 - 1)

-- Check if there are any possible links between two points from sets of points
possibleLinks :: Set -> Set -> [(Point, Point)]
possibleLinks (Set []) (Set ((_, _):_:_)) = []
possibleLinks (Set ((_, _):_:_)) (Set []) = []
possibleLinks (Set []) (Set []) = []
possibleLinks (Set [_]) (Set []) = []
possibleLinks (Set []) (Set [_]) = []
possibleLinks (Set (x:xs)) (Set (y:ys)) = case (neighbors x y) of
                                            True -> (x,y) : possibleLinks (Set xs) (Set ys)
                                            False -> possibleLinks (Set xs) (Set ys) 

-- Get the cardinal direction orientation between two points if there is one
orientation :: Point -> Point -> Maybe Direction
orientation (x1,y1) (x2,y2)
  | y1 == y2 = if x1 > x2 
                 then 
                   Just N 
               else 
                 if x1 < x2 
                   then 
                     Just S 
                 else 
                   Nothing
  | x1 == x2 = if y1 < y2 
                 then
                   Just E 
               else 
                 if y1 > y2 
                   then 
                     Just W 
                  else 
                    Nothing
  | otherwise = Nothing

-- Add an exit to a room
addExit :: Room -> Direction -> Room -> Room
addExit dst direction src = src {exits = (direction, rname dst) : exits src}

-- Create a link between two points
nodeLink :: Point -> Point -> GameGrid -> Maybe GameGrid
nodeLink p1 p2 game_grid
  | Data.Maybe.isNothing (orientation p1 p2) = Nothing
  | orientation p1 p2 == Just N && neighbors p1 p2 = Just (nodeLinker p1 p2 game_grid N)
  | orientation p1 p2 == Just S && neighbors p1 p2 = Just (nodeLinker p1 p2 game_grid S)
  | orientation p1 p2 == Just E && neighbors p1 p2 = Just (nodeLinker p1 p2 game_grid E)
  | orientation p1 p2 == Just W && neighbors p1 p2 = Just (nodeLinker p1 p2 game_grid W)
  | otherwise = Nothing

-- Helper function that actually creates the link
nodeLinker :: Point -> Point -> GameGrid -> Direction ->  GameGrid
nodeLinker p1 p2 game_grid direction = M.insert p2 (addExit (opp_src_to_dst_key) opposite_direction (opp_dst_to_src_key)) partial_game_grid
                                       where
                                         opposite_direction = case direction of
                                                                N -> S
                                                                S -> N
                                                                E -> W
                                                                W -> E
                                         src_to_dst_key = game_grid M.! p2
                                         dst_to_src_key = game_grid M.! p1
                                         partial_game_grid = M.insert p1 (addExit (src_to_dst_key) direction (dst_to_src_key)) game_grid
                                         opp_src_to_dst_key = partial_game_grid M.! p1
                                         opp_dst_to_src_key = partial_game_grid M.! p2

-- Check if there are any neigbhours present between two sets
setNeighbors :: Set -> Set -> Bool
setNeighbors (Set s1) (Set s2) = length possible_neigbhours /= 0
                                 where
                                   possible_neigbhours = possibleLinks (Set s1) (Set s2)

-- Perform the union between two sets
setUnion :: Set -> Set -> Set
setUnion (Set s1) (Set s2) = Set (union s1 s2)

-- Return a new grid if possible with a link made between two points from two sets
setLink :: Set -> Set -> GameGrid -> IO (Maybe GameGrid)
setLink (Set s1) (Set s2) game_grid = do
                                        random_p1 <- choose s1
                                        random_p2 <- choose s2
                                        return $ nodeLink random_p1 random_p2 game_grid

-- Update the sets after forming a link
-- We are using this to remove the edge from the set of edges we originally had since we are finding a spanning tree (no repeated edges or cycles)
updateSetList :: Set -> Set -> [Set] -> [Set]
updateSetList s1 s2 [] = []
updateSetList s1 s2 set_list = (setUnion s1 s2) : (filter (\s -> s /= s1 && s /= s2) set_list)

-- One round of Kruskals algo for a minimum spanning tree
-- Wow... do is really helping me deal with side effectful values
oneRound :: [Set] -> GameGrid -> IO (Maybe GameGrid, Set, Set)
oneRound [] game_grid = return $ (Just game_grid, Set [], Set [])
oneRound list_of_sets game_grid = do
                                    s1 <- choose list_of_sets
                                    s2 <- choose list_of_sets
                                    case (setNeighbors s1 s2) of
                                      True -> do
                                                updated_game_grid <- setLink s1 s2 game_grid
                                                return $ (updated_game_grid, s1, s2)
                                      False -> oneRound list_of_sets game_grid

-- Kruskals algo for a minimum spanning tree
generateMap :: [Set] -> GameGrid -> IO GameGrid
generateMap [] game_grid = return $ game_grid
generateMap [_] game_grid = return $ game_grid
generateMap list_of_sets game_grid = do
                                       (updated_game_grid, s1, s2) <- oneRound list_of_sets game_grid
                                       case updated_game_grid of
                                         Nothing -> generateMap (updateSetList s1 s2 list_of_sets) game_grid
                                         Just game_grid_plus1_connection -> generateMap (updateSetList s1 s2 list_of_sets) game_grid_plus1_connection


-- Create a random map
randomMap :: IO GameGrid
randomMap = do
             base_grid <- (exampleGrid . map zeroExits) allRooms
             let
               initial_set = map mkSet (M.keys base_grid)
             generateMap initial_set base_grid

-- Convert a grid to a game map
-- Same implementation as mkMap from the previous assignment with a small change 
gridToMap :: GameGrid -> GameMap
gridToMap game_grid = M.fromList rooms_lookup_table
                      where
                        makePair room = (rname room, room)
                        rooms_lookup_table = map makePair (M.elems game_grid)

-- Function to perform a shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

-- Choose a random element from a list
choose :: [a] -> IO a
choose lst = do
  index <- System.Random.randomRIO (0, (len - 1))
  return $ head $ drop index $ take (index+1) $ lst
  where
    len = length lst

-- Create a list with randomly generated elements
exampleList :: IO a -> IO Int -> IO [a]
exampleList rand1 rand2 = do 
  seed_for_length <- rand2
  sequence $ L.replicate seed_for_length rand1

class Example a where
  example :: IO a

-- Random example of item
instance Example Item where
    example = do
      weight <- System.Random.randomRIO (0,160)
      choose $ map (\item_name ->  Item item_name weight) itemNames

-- Random example of direction
instance Example Direction where
    example = choose [N, S, E, W]

-- Random example of exit
exitExample :: IO Exit
exitExample = do
  direction <- example :: IO Direction
  room_name <- choose roomNames
  return (direction, room_name)

-- Random example of room
instance Example Room where
    example = do
      room_name <- choose roomNames
      exits <- exampleList exitExample (System.Random.randomRIO (2,4))
      objects <- exampleList (choose itemNames) (System.Random.randomRIO (2,5))
      return $ Room room_name ("This randomly-generated room you're in seems to be the " <> (show room_name) <> "!") exits objects

-- Random example of player
instance Example Player where
  example = do
    objects <- exampleList (choose itemNames) (System.Random.randomRIO (0,10))
    weight <- System.Random.randomRIO (120,200)
    location <- choose roomNames
    return $ Player (L.nub objects) weight location

-- Older version
-- Random example of game state
-- instance Example GameState where
--   example = do
--     message <- choose [Just "WOW!! Everything is so... unique!", Just "Why does everything kind of look the same...?", Nothing]
--     gmap_attr <- exampleList (choose allRooms) (System.Random.randomRIO (2,3))
--     universe_attr <- exampleList (choose [summitStone, trekkingPoles, skis, iceAxes, ropes, flag, snacks, ladders, oxygen, camera]) (System.Random.randomRIO (5,10))
--     player <- example :: IO Player
--     return $ GameState message (mkMap gmap_attr) undefined (mkUniverse universe_attr) player

-- Random example of game state
instance Example GameState  where
    example = do
                random_grid <- randomMap
                return (GameState Nothing (gridToMap random_grid) random_grid univ you)