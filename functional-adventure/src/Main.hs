module Main where

import Control.Monad
import Control.Monad.State

import GameIO
import GameState
import Example

-- Continuously perform state transitions
performStateTransformation :: GameIO ()
performStateTransformation = do
                               current_state <- get
                               next_state <- lift (Control.Monad.State.execStateT GameIO.repl current_state)
                               put next_state
                               Control.Monad.forever performStateTransformation

-- Main program to run the game
-- Comment and uncomment the following two lines and vice versa:
-- 1. "let initial_state = GameState.initialState" to use 
-- 2. "initial_state <- example :: IO GameState"
-- to use the initialState or random state as the starting game state respectively
main :: IO ()
main = do
         let initial_state = GameState.initialState
         -- initial_state <- example :: IO GameState
         Control.Monad.State.evalStateT GameIO.opening initial_state
         Control.Monad.State.execStateT performStateTransformation initial_state
         return ()