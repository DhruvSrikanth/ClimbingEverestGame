module GameIO where

import Control.Monad.State
import System.Exit
import System.IO
import qualified Control.Applicative

import GameState
import Player
import Room
import Command
import Item


type GameIO a = StateT GameState IO a
-- type GameIO = StateT GameState IO -- point-free style #etareduced 

-- Display the current game map
displayMap :: GameIO ()
displayMap = do
               current_state <- get
               let
                 location_ = Just (location $ player current_state)
                 player_on_map = GameState.showGrid location_ gameGrid
                in
                  (lift . putStr) player_on_map

-- Perform some change on the game state and update it
effectChange :: (GameState -> GameState) -> GameIO ()
effectChange transition = do
                            initial_state <- get
                            put $ transition initial_state

-- The initial prompt after which the user can enter their moves
prompt :: GameIO ()
prompt = lift $ putStr "-> " >> hFlush stdout

-- Print a message to the user interface and update the game state
printMessage :: GameIO ()
printMessage = do
                 current_state <- get
                 case (message current_state) of
                   Just message_ -> (lift $ putStrLn message_) >> effectChange (setMessage "")
                   Nothing -> lift $ pure ()

-- Print the description of the room the player is currently in
-- Trying to improve with point free style
printDescription :: GameIO ()
printDescription = do
                     current_state <- get
                     (lift . putStrLn . desc . GameState.currentRoom) current_state


-- Print the objects that are close to the player
-- Is it okay if I used mapM_? I think it comes in Prelude and it is the monadic form of map used when the side effect is carried out rather than caring about the mapped list result
printObjects :: GameIO ()
printObjects = do
                 current_state <- get
                 case (length . GameState.nearbyObjects) current_state of
                   0 -> lift $ pure ()
                   _ -> (lift . putStrLn) "You see the following objects:"
                 mapM_ (lift . putStrLn . show) (GameState.nearbyObjects current_state)


-- Print the exits that are around the player
-- Point free within point free...hmmm this is getting a bit crazy
printExits :: GameIO ()
printExits = do
               current_state <- get
               case (length . exits . GameState.currentRoom) current_state of
                 0 -> lift $ pure ()
                 _ -> (lift . putStrLn) "There are exits in the following directions:"
               ((mapM_ (lift . putStrLn . show . fst)) . (exits . GameState.currentRoom)) current_state


-- Print the inventory of the player
printInventory :: GameIO ()
printInventory = do
                   current_state <- get
                   case (length . GameState.currentInventory) current_state of
                     0 -> (lift . putStrLn) "You aren't carrying anything."
                     _ -> (lift . putStrLn) "You are carrying the following objects:"
                   mapM_ (lift . putStrLn . show) (GameState.currentInventory current_state)


-- Perform an action over multiple items
actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
-- actionOverList action action_items = do
--                                        current_state <- get
--                                        mapM_ (\item -> put (action item current_state) >> printMessage) action_items
actionOverList action action_items = foldl (\game_io item -> game_io >> effectChange (action item) >> printMessage) (pure ()) action_items

-- What the player will see if they die
dieGame :: GameIO ()
dieGame = lift $ putStrLn "You Died! :( You did not pick up the snacks and oxygen...You are in the death zone...you needed the energy to continue...better luck next time..." >> System.Exit.exitSuccess

-- What the player will see if they win
finishGame :: GameIO ()
finishGame = lift $ putStrLn "You did it!!!...You successfully brought your home country's flag to the summit of Mt. Everest...Congrats!...YOU WIN!" >> System.Exit.exitSuccess

-- What the player will see if they decide to exit the game
exit :: GameIO ()
exit = lift $ putStrLn "Goodbye!" >> System.Exit.exitSuccess

-- Condition to check whether the player is going to die
checkGameDie :: GameIO ()
checkGameDie = do
                 current_state <- get
                 case GameState.willDieGame current_state of
                   True -> dieGame
                   False -> lift $ pure ()

-- Condition to check whether the player is going to win
checkGameOver :: GameIO ()
checkGameOver = do
                  current_state <- get
                  case GameState.haveWonGame current_state of
                    True -> finishGame
                    False -> lift $ pure ()

-- What the player will see if they use a command not recognized
syntaxError :: GameIO ()
syntaxError = lift $ putStrLn "I don't understand that."

-- What the player will see at the start of the game
opening :: GameIO ()
opening = lift $ putStrLn "Welcome to Functional Adventure! Let us try to climb the almighty Mt. Everest!!"

-- What the player will see when they use a command
performCommand :: Command -> GameIO ()
performCommand Look = printDescription >> printObjects >> printExits
performCommand Map = displayMap
performCommand Inventory = printInventory
performCommand Exit = exit
performCommand (Take item_list) = actionOverList GameState.takeItem item_list
performCommand (Drop item_list) = actionOverList GameState.dropItem item_list
performCommand (Move direction) = (effectChange . GameState.move) direction >> printMessage 

-- Allow the chaining of commands
performConjunction :: Conjunction -> GameIO ()
performConjunction command_list = mapM_ performCommand command_list

-- Parse multiple commands
parseConjunction :: String -> GameIO ()
parseConjunction input_string = case Command.parseInput input_string of
                                Nothing -> syntaxError
                                Just parse_result -> performConjunction parse_result

-- How the each round will take place between each player move
repl :: GameIO ()
repl = do
         prompt
         user_input <- lift $ getLine
         parseConjunction user_input
         checkGameDie
         checkGameOver